# Build the processed tables for the EV-uptake-by-income analysis.
#
# Reads the raw state registration files and the ATO/ABS income tables listed
# in config.yaml and writes tidy CSVs to processed/:
#
#   lga_income.csv           NSW + QLD LGAs, ABS Personal Income 2022-23 + income group
#   postcode_income.csv      VIC postcodes, ATO Taxation Statistics 2023-24 + income group
#   flow_lga_month.csv       new private registrations by LGA and month (NSW, QLD),
#                            exact counts plus number of suppressed "<=5" cells
#   stock_nsw_lga.csv        NSW light-vehicle fleet by LGA at quarter ends
#   stock_qld_proxy_lga.csv  QLD BEVs seen in transactions since Jan 2022, at last known LGA
#   vic_postcode_quarter.csv VIC fleet by postcode and quarter (stock + recent model years)
#   fuel_prices.csv, fuel_crisis_onset.csv, suppression.csv, adjustments.csv,
#   nsw_unmapped_months.csv
#
# Run:  Rscript R/build_data.R
# common.R sits next to this script: found via Rscript's --file, or source()'s ofile (RStudio's Source button)
source(file.path(local({ f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (!length(f)) f <- sys.frames()[[1]]$ofile
  if (length(f)) dirname(normalizePath(f)) else "R" }), "common.R"))
suppressPackageStartupMessages(library(readxl))

# ---------------------------------------------------------------- income ----
# Earner-weighted income groups: sort by income, cut the cumulative earner
# share into n equal slices; the midpoint of each area's slice decides its
# group, so no group is empty. 1 = lowest income.
income_groups <- function(d, value_col, weight_col, n) {
  d <- d[order(d[[value_col]], method = "radix")]
  w <- d[[weight_col]]
  mid <- cumsum(w) / sum(w) - w / sum(w) / 2
  d[, income_group := pmin(floor(mid * n) + 1L, n)]
  d
}

load_lga_income <- function() {
  c <- CFG$income
  f <- file.path(RAW, c$lga_file)
  hdr <- as.data.table(read_excel(f, sheet = c$lga_sheet, skip = c$lga_header_rows[1], n_max = 2, col_names = FALSE,
                                  .name_repair = "minimal"))
  top <- unlist(hdr[1]); bot <- unlist(hdr[2])
  top <- zoo_fill(top)
  nm <- ifelse(is.na(top), bot, paste0(top, "|", bot))
  d <- as.data.table(read_excel(f, sheet = c$lga_sheet, skip = c$lga_header_rows[2] + 1, col_names = FALSE,
                                .name_repair = "minimal"))
  setnames(d, make.unique(nm[seq_len(ncol(d))]))
  d <- d[!is.na(suppressWarnings(as.numeric(LGA)))]
  d[, lga_code := as.character(as.integer(as.numeric(LGA)))]
  d[, state := fcase(substr(lga_code, 1, 1) == "1", "NSW", substr(lga_code, 1, 1) == "3", "QLD")]
  d <- d[!is.na(state)]
  out <- data.table(state = d$state, lga_code = d$lga_code, lga_name = d[["LGA NAME"]],
                    median_income = suppressWarnings(as.numeric(d[[paste0(c$lga_measure, "|", c$lga_year)]])),
                    earners = suppressWarnings(as.numeric(d[[paste0(c$lga_weight, "|", c$lga_year)]])))
  out <- na.omit(out)
  unin <- startsWith(out$lga_name, "Unincorporated")
  adj("ABS LGA income", "Dropped unincorporated areas", "No council area to match registrations to", sprintf("%d areas", sum(unin)))
  out <- out[!unin]
  adj("ABS LGA income", "Earner-weighted income groups",
      sprintf("LGAs ranked by median total income %s and cut into %d groups each holding ~1/%d of the state's earners (within state)",
              c$lga_year, c$n_groups, c$n_groups), sprintf("%d LGAs", nrow(out)))
  out <- rbindlist(lapply(split(out, out$state), income_groups, "median_income", "earners", c$n_groups))
  setorderv(out, c("state", "lga_name"))
  out
}

# forward-fill a header row (Excel merged cells come through as NA)
zoo_fill <- function(x) { for (i in seq_along(x)[-1]) if (is.na(x[i])) x[i] <- x[i - 1]; x }

load_postcode_income <- function() {
  c <- CFG$income
  d <- as.data.table(read_excel(file.path(RAW, c$postcode_file), sheet = c$postcode_sheet, skip = c$postcode_header_row,
                                .name_repair = "minimal"))
  setnames(d, trimws(gsub("\\s+", " ", names(d))))
  yr <- c$postcode_year
  med <- grep(paste0("^Median.*", yr), names(d), value = TRUE)[1]
  ind <- grep(paste0("^Individuals.*", yr), names(d), value = TRUE)[1]
  st <- grep("^State", names(d), value = TRUE)[1]
  pc <- grep("^Postcode", names(d), value = TRUE)[1]
  d <- d[get(st) == "VIC"]
  out <- data.table(postcode = suppressWarnings(as.numeric(d[[pc]])), median_income = suppressWarnings(as.numeric(d[[med]])),
                    individuals = suppressWarnings(as.numeric(d[[ind]])))
  out <- na.omit(out)
  out[, postcode := as.integer(postcode)]
  v <- CFG$vic
  out <- out[postcode >= v$postcode_min & postcode <= v$postcode_max]
  adj("ATO postcode income", "Kept VIC postcodes only",
      sprintf("State = VIC and postcode %d-%d (drops PO-box-only codes outside the range); ATO omits postcodes with too few individuals",
              v$postcode_min, v$postcode_max), sprintf("%d postcodes", nrow(out)))
  adj("ATO postcode income", "Earner-weighted income groups",
      sprintf("Postcodes ranked by median taxable income %s and cut into %d groups weighted by individuals", yr, c$n_groups),
      sprintf("%d postcodes", nrow(out)))
  out <- income_groups(out, "median_income", "individuals", c$n_groups)
  setorder(out, postcode)
  out
}

# ------------------------------------------------------------ NSW flows ----
fuel_group <- function(labels, bev, phev) fifelse(labels %chin% bev, "bev", fifelse(labels %chin% phev, "phev", "other"))
count_num <- function(x) { v <- suppressWarnings(as.numeric(x)); v[is.na(v)] <- 0; v }

# R's own unzip, not the unzip command, so this also works on Windows
read_zip_member <- function(zipfile, member, select = NULL) {
  tmp <- tempfile(); on.exit(unlink(tmp, recursive = TRUE))
  f <- unzip(zipfile, files = member, exdir = tmp, junkpaths = TRUE)
  fread(f, sep = CFG$nsw$sep, colClasses = "character", select = select, showProgress = FALSE)
}
zip_members <- function(zipfile) unzip(zipfile, list = TRUE)$Name
member_month <- function(m) { p <- strsplit(m, "_")[[1]]; x <- p[length(p) - 1]; paste0(substr(x, 1, 4), "-", substr(x, 5, 6)) }

load_nsw_transactions <- function() {
  n <- CFG$nsw
  zs <- sort(Sys.glob(file.path(RAW, n$transactions_glob)))
  d <- rbindlist(lapply(zs, function(z) rbindlist(lapply(sort(zip_members(z)), function(m) {
    x <- read_zip_member(z, m); x[, month := member_month(m)]; x
  }))))
  logf("NSW transactions: %s rows, %s to %s", comma(nrow(d)), min(d$month), max(d$month))
  d
}

nsw_flows <- function(tx, lga_lookup) {
  n <- CFG$nsw
  is_new <- tx[["VEHICLE REGISTRATION TRANSACTION TYPE"]] == n$new_transaction
  adj("NSW new registrations", "Kept new-vehicle registrations only",
      sprintf("Transaction type '%s'; transfers and second-hand re-registrations dropped", n$new_transaction),
      sprintf("%s of %s rows kept", comma(sum(is_new)), comma(nrow(tx))))
  priv <- is_new & tx[["CUSTOMER TYPE"]] %chin% n$private_customer_types
  adj("NSW new registrations", "Private customers only",
      sprintf("Customer type in %s; Business, Dealer (demonstrators) and Government dropped", listing(n$private_customer_types)),
      sprintf("%s rows dropped", comma(sum(is_new & !priv))))
  bad <- priv & tx[["FUEL TYPE"]] %chin% n$exclude_fuel_labels
  unm <- tx[bad & tx[["FUEL TYPE"]] == "Other/Unmapped", .N, by = month]
  gap <- sort(unm[N > n$unmapped_block_min_rows, month])
  fwrite(data.table(month = gap), file.path(OUT, "nsw_unmapped_months.csv"))
  adj("NSW new registrations", "Dropped rows with no usable fuel type",
      sprintf("Fuel type in %s (trailers/caravans and unmapped rows). A block of 'Other/Unmapped' rows (manufacturer also unmapped) appears in %s; assumed spread across fuels like mapped rows",
              listing(n$exclude_fuel_labels), paste(gap, collapse = ", ")),
      sprintf("%s rows dropped", comma(sum(bad))))
  adj("NSW new registrations", "Harmonised fuel labels",
      sprintf("TfNSW changed labels twice. BEV = %s; PHEV = %s; everything else = other", listing(n$bev_labels), listing(n$phev_labels)), "")
  d <- tx[priv & !bad]
  d[, fuel := fuel_group(`FUEL TYPE`, n$bev_labels, n$phev_labels)]
  d[, supp := as.integer(COUNT == n$suppressed_token)]
  d[, exact := count_num(COUNT)]
  alias <- unlist(n$lga_aliases)
  d[, lga_name := fifelse(`CUSTOMER ADDRESS LGA` %chin% names(alias), alias[`CUSTOMER ADDRESS LGA`], `CUSTOMER ADDRESS LGA`)]
  adj("NSW new registrations", "LGA names mapped to ABS",
      sprintf("Aliases %s", paste(sprintf("'%s' -> '%s'", names(alias), alias), collapse = "; ")),
      sprintf("%s rows relabelled", comma(sum(d[["CUSTOMER ADDRESS LGA"]] %chin% names(alias)))))
  miss <- !d$lga_name %chin% lga_lookup
  adj("NSW new registrations", "Dropped rows with no matchable LGA",
      sprintf("Labels: %s", listing(sort(unique(d$lga_name[miss])))), sprintf("%s rows dropped", comma(sum(miss))))
  d <- d[!miss]
  adj("NSW new registrations", "Suppressed counts imputed",
      sprintf("Counts of 5 or fewer are published as '%s'; each such cell is valued at an estimated mean (see Suppression sheet; editable on Inputs)",
              n$suppressed_token),
      sprintf("%s of %s rows (%s) suppressed", comma(sum(d$supp)), comma(nrow(d)), pct1(mean(d$supp))))
  list(flows = wide_by_fuel(d[, .(exact = sum(exact), supp = sum(supp)), by = .(lga_name, month, fuel)], c("lga_name", "month"))[
         , state := "NSW"][], rows = d)
}

# long (keys, fuel, exact, supp) -> wide bev_exact, bev_supp, phev_exact, ...
wide_by_fuel <- function(g, keys) {
  w <- dcast(g, as.formula(paste(paste(keys, collapse = "+"), "~ fuel")), value.var = c("exact", "supp"), fill = 0)
  for (f in FUEL_GROUPS) for (m in c("exact", "supp")) {
    src <- paste0(m, "_", f)
    w[, (paste0(f, "_", m)) := if (src %in% names(w)) get(src) else 0]
  }
  w[, c(keys, as.vector(outer(c("_exact", "_supp"), FUEL_GROUPS, function(a, b) paste0(b, a)))), with = FALSE]
}

# -------------------------------------------------------------- QLD ----
load_qld <- function() {
  q <- CFG$qld
  cols <- c("RECORD_DATE", "OPEN_DATA_VEHICLE_IDENTIFIER", "TRANSACTION_TYPE", "CUSTOMER_TYPE",
            "LGA_NAME", "MAKE", "COLOUR", "FUEL_TYPE", "YEAR_OF_MANUFACTURE")
  # A few records carry a backslash-escaped comma inside a field ("\\,") and stray
  # double quotes; strip the escape and read without quote handling so no row is lost.
  rd <- function(f) fread(cmd = sprintf("sed 's/\\\\,/ /g' %s", shQuote(f)), select = cols, colClasses = "character",
                          quote = "", showProgress = FALSE)
  d <- rbindlist(lapply(sort(Sys.glob(file.path(RAW, q$glob))), rd))
  before <- nrow(d)
  d <- unique(d)
  logf("QLD transactions: %s rows, %s exact duplicates dropped", comma(before), comma(before - nrow(d)))
  adj("QLD registrations", "Dropped exact duplicate records", "Identical on every field read",
      sprintf("%s of %s rows", comma(before - nrow(d)), comma(before)))
  d[, date := as.IDate(RECORD_DATE)]
  d[, month := substr(RECORD_DATE, 1, 7)]
  d[, lga_name := trimws(gsub(q$lga_suffix_regex, "", LGA_NAME, perl = TRUE))]
  adj("QLD registrations", "LGA names mapped to ABS", "Stripped council-type suffix, e.g. 'Brisbane (C)' -> 'Brisbane'", "all rows")
  d[, fuel := fuel_group(FUEL_TYPE, unlist(q$bev_labels), unlist(q$phev_labels))]
  adj("QLD registrations", "Fuel groups",
      sprintf("BEV = %s. 'Petrol And Electric' mixes HEV and PHEV so it stays in 'other'", listing(unlist(q$bev_labels))), "")
  d
}

qld_new_private <- function(d) {
  q <- CFG$qld
  yom <- suppressWarnings(as.numeric(d$YEAR_OF_MANUFACTURE))
  new <- d$TRANSACTION_TYPE == q$new_transaction
  priv <- new & d$CUSTOMER_TYPE %chin% q$private_customer_types
  young <- priv & !is.na(yom) & yom >= year(d$date) - q$max_new_vehicle_age_years
  adj("QLD registrations", "Kept 'Registration New' only", "Transfers dropped", sprintf("%s of %s rows kept", comma(sum(new)), comma(nrow(d))))
  adj("QLD registrations", "Private customers only", sprintf("Customer type %s; Organisation dropped", listing(q$private_customer_types)),
      sprintf("%s rows dropped", comma(sum(new & !priv))))
  adj("QLD registrations", "Dropped re-registrations of older vehicles",
      sprintf("'Registration New' also covers used vehicles coming back on the register; kept only year of manufacture >= registration year - %d",
              q$max_new_vehicle_age_years), sprintf("%s rows dropped", comma(sum(priv & !young))))
  d[young]
}

qld_last_complete_month <- function(d) {
  last <- max(d$date)
  m <- format(last, "%Y-%m")
  month_end <- as.IDate(seq(as.Date(paste0(m, "-01")), by = "month", length.out = 2)[2] - 1)
  if (last >= month_end - CFG$qld$month_complete_tolerance_days) m else int_to_ym(ym_to_int(m) - 1L)
}

qld_flows <- function(new, lga_lookup, last_month) {
  part <- new$month > last_month
  adj("QLD registrations", "Dropped incomplete latest month",
      sprintf("Extract ends %s; months after %s are partial", format(max(new$date), "%d %b %Y"), last_month), sprintf("%s rows", comma(sum(part))))
  new <- new[lga_name %chin% lga_lookup & !part]
  g <- new[, .(exact = .N, supp = 0L), by = .(lga_name, month, fuel)]
  wide_by_fuel(g, c("lga_name", "month"))[, state := "QLD"][]
}

# BEVs seen in any QLD transaction since the data start, placed at the LGA of
# their latest transaction up to each month end.
qld_stock_proxy <- function(d, lga_lookup) {
  b <- d[fuel == "bev"]
  b[, ord := .I]
  setorder(b, date, ord)
  months <- ym_seq(min(b$month), max(b$month))
  out <- rbindlist(lapply(months, function(m) {
    last <- b[month <= m, .SD[.N], by = OPEN_DATA_VEHICLE_IDENTIFIER, .SDcols = "lga_name"]
    last[, .(bev_seen = .N), by = lga_name][, month := m][]
  }))
  adj("QLD fleet (proxy)", "Built a BEV stock proxy from transactions",
      sprintf("QLD publishes no current fleet-by-fuel data by region. Each BEV appearing in any new or transfer record since %s is placed at the LGA of its latest record up to each month. Misses pre-2022 BEVs never transferred; keeps BEVs later written off or moved interstate",
              min(b$month)), sprintf("%s distinct BEVs", comma(uniqueN(b$OPEN_DATA_VEHICLE_IDENTIFIER))))
  out[lga_name %chin% lga_lookup, .(lga_name, month, bev_seen)]
}

# ------------------------------------------------ suppression estimate ----
# Mean size of a '<=5' cell at NSW's grain, by fuel group. NSW cross-classifies
# each new registration by LGA x make x fuel x colour x gender x age group
# before suppressing counts <=5. QLD publishes unit records with LGA, make,
# fuel and colour but no gender/age. Each QLD vehicle gets a synthetic
# gender x age group drawn from the NSW private new-vehicle mix for its fuel
# group; aggregate to NSW's grain and average the cells NSW would suppress.
estimate_suppression <- function(nsw_rows, qld_new) {
  n <- CFG$nsw
  set.seed(n$suppression_seed)
  mix <- nsw_rows[, .N, by = .(fuel, GENDER, `AGE GROUP`)]
  q <- copy(qld_new)
  q[, c("GENDER", "AGE GROUP") := ""]
  for (f in FUEL_GROUPS) {
    idx <- which(q$fuel == f)
    p <- mix[fuel == f]
    if (!length(idx) || !nrow(p)) next
    pick <- sample.int(nrow(p), length(idx), replace = TRUE, prob = p$N)
    set(q, idx, "GENDER", p$GENDER[pick])
    set(q, idx, "AGE GROUP", p$`AGE GROUP`[pick])
  }
  token_max <- as.integer(gsub("\\D", "", n$suppressed_token))
  cells <- q[, .N, by = .(month, lga_name, MAKE, fuel, COLOUR, GENDER, `AGE GROUP`)]
  out <- cells[, .(mean_suppressed_cell = round(mean(N[N <= token_max]), 3), share_of_cells_suppressed = round(mean(N <= token_max), 3)),
               by = .(fuel_group = fuel)]
  setorder(out, fuel_group)
  logf("Suppressed-cell estimate:"); print(out)
  out
}

# Imputed value for '<=5' cells in the NSW fleet snapshot (see config)
calibrate_stock_suppression <- function(tx, statewide, register, k_flow_bev) {
  n <- CFG$nsw
  first <- min(statewide$month); last <- max(statewide$month)
  b <- tx[`VEHICLE REGISTRATION TRANSACTION TYPE` == n$new_transaction & `FUEL TYPE` %chin% n$bev_labels & month > first & month <= last]
  new_bev <- sum(suppressWarnings(as.numeric(b$COUNT)), na.rm = TRUE) + k_flow_bev * sum(b$COUNT == n$suppressed_token)
  s1 <- statewide[month == last]; s0 <- statewide[month == first]
  d_exact <- s1$bev_exact - s0$bev_exact
  d_supp <- s1$bev_supp - s0$bev_supp
  k_bev <- (new_bev - d_exact) / d_supp
  z <- file.path(RAW, n$age_snapshot_file)
  tag <- gsub("-", "", n$age_calibration_month)
  a <- rbindlist(lapply(grep(paste0("_", tag, "_"), zip_members(z), value = TRUE), function(m) read_zip_member(z, m)))
  age_total <- sum(suppressWarnings(as.numeric(a$COUNT)), na.rm = TRUE) + n$age_file_suppressed_value * sum(a$COUNT == n$suppressed_token)
  k_other <- (age_total - register$exact) / register$supp
  logf("Stock calibration: new BEV %s..%s = %s; BEV exact growth %s; suppressed BEV cells growth %s -> k_bev %.3f",
       first, last, comma(new_bev), comma(d_exact), comma(d_supp), k_bev)
  logf("  register %s: age-file total %s, detailed exact %s, suppressed cells %s -> k_other %.3f",
       tag, comma(age_total), comma(register$exact), comma(register$supp), k_other)
  c(bev = k_bev, other = k_other)
}

# ------------------------------------------------------------ NSW stock ----
nsw_stock <- function(lga_lookup) {
  n <- CFG$nsw
  files <- rbindlist(lapply(sort(Sys.glob(file.path(RAW, n$snapshot_glob))), function(z)
    data.table(zip = z, member = zip_members(z))))
  files[, month := vapply(member, member_month, "")]
  latest <- max(files$month)
  calib <- n$age_calibration_month
  keep <- unique(c(files[as.integer(substr(month, 6, 7)) %in% n$snapshot_months_of_year, month], latest, calib))
  setorder(files, month, member)
  register <- list(exact = 0, supp = 0)
  alias <- unlist(n$lga_aliases)
  rows <- list()
  for (i in which(files$month %in% keep)) {
    f <- files[i]
    d <- read_zip_member(f$zip, f$member, select = c("VEHICLE TYPE", "MOTIVE POWER", "CUSTOMER ADDRESS LGA", "COUNT"))
    if (f$month == calib) {
      register$exact <- register$exact + sum(suppressWarnings(as.numeric(d$COUNT)), na.rm = TRUE)
      register$supp <- register$supp + sum(d$COUNT == n$suppressed_token)
    }
    d <- d[`VEHICLE TYPE` %chin% n$snapshot_vehicle_types & !`MOTIVE POWER` %chin% n$snapshot_exclude_power]
    if (!nrow(d)) next
    d[, fuel := fuel_group(`MOTIVE POWER`, n$snapshot_bev_labels, n$snapshot_phev_labels)]
    d[, lga_name := fifelse(`CUSTOMER ADDRESS LGA` %chin% names(alias), alias[`CUSTOMER ADDRESS LGA`], `CUSTOMER ADDRESS LGA`)]
    rows[[length(rows) + 1]] <- d[, .(exact = sum(count_num(COUNT)), supp = sum(COUNT == n$suppressed_token)), by = .(lga_name, fuel)][
      , month := f$month][]
    logf("  NSW snapshot %s: %s light-vehicle rows", f$member, comma(nrow(d)))
  }
  adj("NSW fleet", "Light vehicles only",
      sprintf("Vehicle types %s; trailers, motorcycles, plant, buses and medium/heavy trucks excluded; motive power %s excluded",
              listing(n$snapshot_vehicle_types), listing(n$snapshot_exclude_power)), "")
  adj("NSW fleet", "Quarter-end snapshots", sprintf("Months %s plus the latest; %d snapshot files read",
                                                   paste0("[", paste(n$snapshot_months_of_year, collapse = ", "), "]"), length(rows)),
      sprintf("%d snapshot months", length(keep)))
  adj("NSW fleet", "Suppressed counts imputed", "BEV and other-fuel cells valued by calibration (see Suppression sheet)", "")
  s <- wide_by_fuel(rbindlist(rows)[, .(exact = sum(exact), supp = sum(supp)), by = .(lga_name, month, fuel)], c("lga_name", "month"))
  statewide <- s[, .(bev_exact = sum(bev_exact), bev_supp = sum(bev_supp)), by = month]
  list(stock = s[lga_name %chin% lga_lookup], statewide = statewide, register = register)
}

# -------------------------------------------------------------- VIC ----
vic_quarters <- function(pc_lookup) {
  v <- CFG$vic
  rows <- lapply(sort(Sys.glob(file.path(RAW, v$glob))), function(f) {
    m <- regmatches(f, regexec("_q(\\d)_(\\d{4})\\.csv$", f))[[1]]
    qtr <- paste0(m[3], "Q", m[2]); yr <- as.integer(m[3])
    d <- fread(f, colClasses = "character", encoding = "UTF-8", showProgress = FALSE)
    setnames(d, trimws(sub("^﻿", "", names(d))))
    d <- d[trimws(CD_CLASS_VEH) %chin% v$vehicle_classes]
    d[, fuel_code := trimws(fifelse(is.na(CD_CL_FUEL_ENG), "", CD_CL_FUEL_ENG))]
    d <- d[!fuel_code %chin% unlist(v$exclude_fuel_codes)]
    d[, n := count_num(TOTAL1)]
    d[, recent := !is.na(suppressWarnings(as.numeric(NB_YEAR_MFC_VEH))) & as.numeric(NB_YEAR_MFC_VEH) >= yr - v$recent_model_year_lag]
    d[, isbev := fuel_code == v$bev_code]
    g <- d[, .(vehicles = sum(n), bev = sum(n * isbev), hybrid = sum(n * (fuel_code == v$hybrid_code)),
               recent_vehicles = sum(n * recent), recent_bev = sum(n * (recent & isbev))),
           by = .(postcode = suppressWarnings(as.integer(POSTCODE)))]
    g[, quarter := qtr]
    logf("  VIC snapshot %s: %s vehicles, %s BEV", qtr, comma(sum(g$vehicles)), comma(sum(g$bev)))
    g
  })
  out <- rbindlist(rows)[!is.na(postcode)]
  keep <- out$postcode %in% pc_lookup
  adj("VIC fleet", "Vehicle class and fuel filter",
      sprintf("Class %s (motor vehicles; motorcycles excluded); blank fuel code excluded. BEV = code '%s'; code '%s' = hybrids (HEV+PHEV, not separable)",
              listing(v$vehicle_classes), v$bev_code, v$hybrid_code), "")
  adj("VIC fleet", "Recent-model proxy for new take-up",
      sprintf("VIC's monthly new-registration file has no location, so 'recent-model' vehicles (year of manufacture >= snapshot year - %d) in each quarterly snapshot stand in for recent new-vehicle take-up",
              v$recent_model_year_lag), "")
  lost <- out[!keep & quarter == max(quarter), sum(vehicles)]
  adj("VIC fleet", "Dropped postcodes with no ATO income", "Postcodes absent from ATO Table 8 (too few taxpayers) or outside the VIC range",
      sprintf("%s postcode-quarters; %s vehicles in latest quarter", comma(sum(!keep)), comma(lost)))
  out <- out[keep, .(postcode, vehicles, bev, hybrid, recent_vehicles, recent_bev, quarter)]
  setorder(out, postcode, quarter)
  out
}

# ------------------------------------------------------------- fuel ----
fuel_prices <- function() {
  f <- CFG$fuel
  r <- fread(file.path(HERE, f$retail_file), select = c("month", f$retail_series))
  t <- fread(file.path(HERE, f$tgp_file), select = c("month", f$tgp_series))
  d <- merge(r, t, by = "month", all = TRUE)
  setorder(d, month)
  d <- d[complete.cases(d[, f$retail_series, with = FALSE])]
  s <- d[[f$onset_series]]
  trail <- frollmean(shift(s, 1), f$onset_trailing_months)
  rise <- s / trail - 1
  i <- which(rise >= f$onset_threshold & d$month >= f$onset_search_from)[1]
  onset <- d$month[i]
  adj("Fuel prices", "Crisis onset detected from prices",
      sprintf("First month from %s where %s is >= %d%% above its trailing %d-month mean", f$onset_search_from, f$onset_series,
              round(100 * f$onset_threshold), f$onset_trailing_months), sprintf("onset %s", onset))
  logf("Fuel crisis onset: %s (%s %.1f c/L, %.0f%% above trailing %d-month mean %.1f)",
       onset, f$onset_series, s[i], 100 * rise[i], f$onset_trailing_months, trail[i])
  list(prices = d, onset = onset, price = s[i], trail = trail[i])
}

# ------------------------------------------------------------------ main ----
main <- function() {
  lga <- load_lga_income()
  write_out(lga, "lga_income.csv")
  logf("LGA income: %s", paste(names(table(lga$state)), table(lga$state), collapse = ", "))
  pc <- load_postcode_income()
  write_out(pc, "postcode_income.csv")
  logf("VIC postcodes with ATO income: %d", nrow(pc))

  start <- CFG$period$start_month
  fp <- fuel_prices()
  write_out(fp$prices[month >= start], "fuel_prices.csv")
  write_out(data.table(onset_month = fp$onset, onset_series = CFG$fuel$onset_series, onset_price = round(fp$price, 1),
                       trailing_mean = round(fp$trail, 1), threshold = CFG$fuel$onset_threshold), "fuel_crisis_onset.csv")

  tx <- load_nsw_transactions()
  nsw_lgas <- lga[state == "NSW", lga_name]
  nf <- nsw_flows(tx, nsw_lgas)
  alias <- unlist(CFG$nsw$lga_aliases)
  lab <- unique(tx[["CUSTOMER ADDRESS LGA"]]); lab <- fifelse(lab %chin% names(alias), alias[lab], lab)
  logf("NSW LGA labels not matched to ABS (dropped): %s", listing(sort(setdiff(lab, nsw_lgas))))

  q <- load_qld()
  qld_lgas <- lga[state == "QLD", lga_name]
  logf("QLD LGA labels not matched to ABS (dropped): %s", listing(sort(setdiff(unique(na.omit(q$lga_name)), qld_lgas))))
  qnew <- qld_new_private(q)
  q_last <- qld_last_complete_month(q)
  logf("QLD last complete month: %s", q_last)
  qf <- qld_flows(qnew, qld_lgas, q_last)

  supp <- estimate_suppression(nf$rows, qnew[lga_name %chin% qld_lgas])
  k_flow <- setNames(supp$mean_suppressed_cell, supp$fuel_group)
  k_flow["phev"] <- k_flow[[CFG$nsw$phev_suppression_from]]

  flows <- rbindlist(list(nf$flows, qf), use.names = TRUE)
  adj("Both states", "Analysis window", sprintf("New-registration analysis starts %s (NSW data begin Jul 2022, QLD Jan 2022)", start), "")
  flows <- flows[month >= start]
  setcolorder(flows, c("state", "lga_name", "month"))
  setorder(flows, state, lga_name, month)
  write_out(flows, "flow_lga_month.csv")
  logf("Flows: %s LGA-months, %s to %s", comma(nrow(flows)), min(flows$month), max(flows$month))

  qs <- qld_stock_proxy(q[month <= q_last], qld_lgas)
  write_out(qs, "stock_qld_proxy_lga.csv")
  rm(q); invisible(gc())

  ns <- nsw_stock(nsw_lgas)
  setorder(ns$stock, lga_name, month)
  write_out(ns$stock, "stock_nsw_lga.csv")
  ks <- calibrate_stock_suppression(tx, ns$statewide, ns$register, k_flow[["bev"]])
  rm(tx); invisible(gc())
  k_stock <- c(bev = ks[["bev"]], other = ks[["other"]])
  k_stock["phev"] <- k_stock[[CFG$nsw$phev_suppression_from]]
  method <- c(flow.bev = "QLD unit records re-cut at NSW grain (synthetic gender x age from NSW mix)",
              flow.other = "QLD unit records re-cut at NSW grain (synthetic gender x age from NSW mix)",
              flow.phev = "Borrowed from flow BEV (QLD has no PHEV label)",
              stock.bev = "NSW BEV fleet growth = new BEV registrations over the same period",
              stock.other = "Detailed snapshot total = coarse age-of-vehicles snapshot total",
              stock.phev = "Borrowed from stock BEV")
  sup <- rbindlist(lapply(c("flow", "stock"), function(t) {
    k <- if (t == "flow") k_flow else k_stock
    data.table(table = t, fuel_group = FUEL_GROUPS, k = round(unname(k[FUEL_GROUPS]), 3), method = method[paste0(t, ".", FUEL_GROUPS)])
  }))
  write_out(sup, "suppression.csv")

  vq <- vic_quarters(pc$postcode)
  write_out(vq, "vic_postcode_quarter.csv")
  write_out(adjustments(), "adjustments.csv")
  logf("done")
}

main()
