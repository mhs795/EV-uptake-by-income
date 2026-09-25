# Shared set-up for the EV-uptake-by-income R scripts: project paths, config,
# logging and the data-adjustments log.
suppressPackageStartupMessages({
  library(data.table)
  library(yaml)
})

# Project root = folder above R/ (works from Rscript and from source())
find_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) {
    return(normalizePath(file.path(dirname(f), "..")))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(normalizePath(file.path(dirname(sys.frames()[[1]]$ofile), "..")))
  }
  # line-by-line in RStudio: walk up from the working directory to the folder holding config.yaml
  d <- normalizePath(".")
  while (!file.exists(file.path(d, "config.yaml")) && dirname(d) != d) d <- dirname(d)
  if (!file.exists(file.path(d, "config.yaml"))) stop("Can't find the project folder: open ev_uptake_income.Rproj in RStudio, or setwd() to the project first")
  d
}
HERE <- Sys.getenv("EV_ROOT", find_root())
CFG <- yaml::read_yaml(file.path(HERE, "config.yaml"))
RAW <- file.path(HERE, CFG$paths$raw)
OUT <- file.path(HERE, Sys.getenv("EV_PROCESSED", CFG$paths$processed))
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

FUEL_GROUPS <- c("bev", "phev", "other")
# label given to private buyers in both states (config nsw/qld customer_types)
PRIVATE <- CFG$nsw$customer_types[[CFG$nsw$private_customer_types[[1]]]]

logf <- function(...) {
  cat(sprintf(...), "\n", sep = "")
  flush.console()
}
comma <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = ",")
pct1 <- function(x) sprintf("%.1f%%", 100 * x)
listing <- function(x) paste0("[", paste0("'", x, "'", collapse = ", "), "]")

# ---- data-adjustments log (written to processed/adjustments.csv) ----------
.ADJ <- new.env()
.ADJ$rows <- list()
adj <- function(dataset, step, detail, affected = "") {
  .ADJ$rows[[length(.ADJ$rows) + 1]] <- data.table(dataset = dataset, adjustment = step, detail = detail, affected = affected)
}
adjustments <- function() rbindlist(.ADJ$rows)

# Months are carried as "YYYY-MM" strings; these helpers do month arithmetic
ym_to_int <- function(x) {
  y <- as.integer(substr(x, 1, 4))
  m <- as.integer(substr(x, 6, 7))
  y * 12L + m - 1L
}
int_to_ym <- function(i) sprintf("%04d-%02d", i %/% 12L, i %% 12L + 1L)
ym_seq <- function(a, b) int_to_ym(seq(ym_to_int(a), ym_to_int(b)))

write_out <- function(dt, name) fwrite(dt, file.path(OUT, name))

# ---- income by council area (ABS Personal Income, Table 1.5) ----------------
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

# forward-fill a header row (Excel merged cells come through as NA)
zoo_fill <- function(x) {
  for (i in seq_along(x)[-1]) if (is.na(x[i])) x[i] <- x[i - 1]
  x
}

# Every council area in Australia: state, code, name, median income and earners
read_lga_income <- function() {
  c <- CFG$income
  f <- file.path(RAW, c$lga_file)
  hdr <- as.data.table(readxl::read_excel(f,
    sheet = c$lga_sheet, skip = c$lga_header_rows[1], n_max = 2, col_names = FALSE,
    .name_repair = "minimal"
  ))
  top <- zoo_fill(unlist(hdr[1]))
  bot <- unlist(hdr[2])
  nm <- ifelse(is.na(top), bot, paste0(top, "|", bot))
  d <- as.data.table(readxl::read_excel(f,
    sheet = c$lga_sheet, skip = c$lga_header_rows[2] + 1, col_names = FALSE,
    .name_repair = "minimal"
  ))
  setnames(d, make.unique(nm[seq_len(ncol(d))]))
  d <- d[!is.na(suppressWarnings(as.numeric(LGA)))]
  d[, lga_code := as.character(as.integer(as.numeric(LGA)))]
  st <- unlist(CFG$context$states)
  d[, state := unname(st[substr(lga_code, 1, 1)])]
  d <- d[!is.na(state)]
  out <- data.table(
    state = d$state, lga_code = d$lga_code, lga_name = d[["LGA NAME"]],
    median_income = suppressWarnings(as.numeric(d[[paste0(c$lga_measure, "|", c$lga_year)]])),
    earners = suppressWarnings(as.numeric(d[[paste0(c$lga_weight, "|", c$lga_year)]]))
  )
  na.omit(out)
}

# ---- how the income groups are built (shared by workbook and dashboard) ----
# Plain-English steps; the numbers come from config so they track any change.
income_group_method <- function() {
  c <- CFG$income
  n <- c$n_groups
  c(
    sprintf(
      "Income measure. Council areas (LGAs), every state: each area's median total income of earners, %s, from ABS Personal Income in Australia (ATO tax data plus other administrative data), Table 1.5. VIC: each postcode's median taxable income, %s, from ATO Taxation Statistics, Individuals Table 8. It is the income of the typical earner living in the area, not household income or wealth.",
      c$lga_year, c$postcode_year
    ),
    "Ranked within each state. Every state is grouped separately, so 'Q5' means the highest-income areas of that state; the dollar cut-offs differ between states. 'All states' shows each state's own groups side by side, and Australia pools them.",
    sprintf(
      "Weighted by people, not by areas. Areas are lined up from lowest to highest median income and their earners are added up in that order (VIC: individuals lodging a return). The running total is cut into %d equal slices, so each group holds about 1/%d of the state's earners. Q1 therefore contains many small, mostly rural areas and Q%d only a few large metropolitan ones.",
      n, n, n
    ),
    "Whole areas, never split. An area goes into the slice that contains the midpoint of its earners, so no area is divided between groups and no group is empty. Group sizes are therefore only roughly equal: one very large area can make its group bigger and a neighbouring group smaller (see the table of actual shares).",
    "Group results are pooled, not averaged. A group's BEV share is its total BEVs divided by its total new registrations (or fleet), so big areas count in proportion to their size.",
    "Area income, not buyer income. Groups describe where a vehicle is registered. A Q1 area has high-income residents and a Q5 area low-income ones; retiree areas have low taxable income but not necessarily low wealth."
  )
}

# One row per income group: size, income range and the largest areas in it.
income_group_composition <- function(d, name_col, income_col, weight_col) {
  k <- CFG$income$examples_per_group
  d <- copy(d)[, `:=`(nm = get(name_col), inc = get(income_col), w = get(weight_col))]
  tot <- sum(d$w)
  d[order(income_group, -w), .(
    areas = .N, people = sum(w), share = sum(w) / tot, inc_lo = min(inc), inc_hi = max(inc),
    largest = paste(utils::head(nm, k), collapse = "; ")
  ), keyby = .(group = income_group)]
}

# "3121 Cremorne, Burnley, Richmond": postcode plus its suburbs (config overrides win)
postcode_label <- function(pc, subs) {
  ov <- unlist(CFG$map$postcode_name_overrides)
  nm <- fcoalesce(ov[as.character(pc)], subs$suburbs[match(pc, subs$postcode)], "")
  trimws(paste(pc, nm))
}

# Sentence on why group shares differ from 1/n in this state.
income_group_lumpiness <- function(d, state, name_col, weight_col) {
  n <- CFG$income$n_groups
  w <- d[[weight_col]]
  tot <- sum(w)
  gs <- tapply(w, d$income_group, sum) / tot
  top <- d[which.max(w)]
  sh <- max(w) / tot
  if (nrow(d) == 1) {
    return(sprintf("%s: a single area (%s), so all its earners fall in one group.", state, top[[name_col]]))
  }
  if (sh < 1 / n) {
    return(sprintf(
      "%s: the largest area (%s) holds only %s of earners, so group shares differ from 1/%d just because whole areas are kept together (%s to %s).",
      state, top[[name_col]], pct1(sh), n, pct1(min(gs)), pct1(max(gs))
    ))
  }
  sprintf(
    "%s: %s alone holds %s of the state's earners, more than a 1/%d slice. It cannot be split, so its group (Q%d) holds %s of earners and Q%s is left with only %s.",
    state, top[[name_col]], pct1(sh), n, top$income_group, pct1(gs[as.character(top$income_group)]),
    names(gs)[which.min(gs)], pct1(min(gs))
  )
}

# ---- analysis windows, spelled out as months ("Sep 2025–Aug 2026") ----------
# Recent window = the latest N months of each state's new-registration data;
# baseline = the first N months of data. N = period.recent_window_months.
ym_long <- function(ym) format(as.Date(paste0(ym, "-01")), "%b %Y")
span_label <- function(a, b) paste0(ym_long(a), "–", ym_long(b))
analysis_windows <- function(flow) {
  n <- CFG$period$recent_window_months
  w <- flow[, .(last = max(month), first = min(month)), keyby = state]
  w[, `:=`(recent_start = int_to_ym(ym_to_int(last) - n + 1L), base_end = int_to_ym(ym_to_int(first) + n - 1L))]
  lab <- function(a, b) {
    l <- span_label(a, b)
    if (uniqueN(l) == 1) l[1] else paste(w$state, l, collapse = "; ")
  }
  list(by_state = w, recent = lab(w$recent_start, w$last), base = lab(w$first, w$base_end), months = n)
}
