# Area context for every council area in Australia (and each VIC postcode):
# the BEV fleet (BITRE, all states), rooftop solar and home batteries (CER) and
# public EV charging stations (OpenStreetMap), with Census people and dwellings.
#
#   lga_context.csv              one row per council area, all states: income, income group,
#                                people, dwellings, BITRE fleet by year, solar, batteries, chargers
#   vic_postcode_context.csv     the same measures for each VIC postcode
#   solar_battery_lga_month.csv  monthly solar and battery installations by council area
#   chargers.csv                 each public charging station, with its council area
#
# Postcode data (BITRE, CER) are shared out to council areas by the Census 2021 people
# or dwellings in each postcode's mesh blocks (the ABS's own correspondence method).
#
# Run:  Rscript R/build_context.R   (after build_data.R and fetch_boundaries.R)
# common.R sits next to this script: found via Rscript's --file, or source()'s ofile (RStudio's Source button)
source(file.path(local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (!length(f)) f <- sys.frames()[[1]]$ofile
  if (length(f)) dirname(normalizePath(f)) else "R"
}), "common.R"))
suppressPackageStartupMessages({
  library(readxl)
  library(sf)
  library(jsonlite)
  library(httr2)
})
X <- CFG$context
`%||%` <- function(a, b) if (is.null(a)) b else a
NG <- CFG$income$n_groups
V <- CFG$vic

# ---- income + income groups, every council area -----------------------------------
inc <- read_lga_income()
unin <- startsWith(inc$lga_name, "Unincorporated") & !(inc$lga_name %chin% unlist(X$keep_unincorporated))
adj(
  "ABS LGA income (all states)", "Dropped unincorporated areas",
  sprintf("No council; kept %s, which covers the whole ACT", paste(unlist(X$keep_unincorporated), collapse = ", ")),
  sprintf("%d areas", sum(unin))
)
inc <- inc[!unin]
inc <- rbindlist(lapply(split(inc, inc$state), income_groups, "median_income", "earners", NG))
adj(
  "ABS LGA income (all states)", "Earner-weighted income groups in every state",
  sprintf("Same method as NSW and QLD: ranked within state, %d groups of ~equal earners. The ACT is one area, so it sits in a single group", NG),
  sprintf("%d areas", nrow(inc))
)

# ---- mesh blocks: people, dwellings, postcode and council area ----------------------
logf("reading ABS mesh blocks")
read_mb_counts <- function() {
  f <- file.path(RAW, X$mb_counts_file)
  sh <- grep("^Table [0-9]", excel_sheets(f), value = TRUE)
  rbindlist(lapply(sh, function(s) {
    d <- as.data.table(read_excel(f, sheet = s, skip = 6, col_types = "text"))
    d[grepl("^[0-9]+$", MB_CODE_2021), .(mb = MB_CODE_2021, persons = fcoalesce(as.numeric(Person), 0), dwellings = fcoalesce(as.numeric(Dwelling), 0))]
  }))
}
mb <- read_mb_counts()
poa <- as.data.table(read_excel(file.path(RAW, X$mb_poa_file), col_types = "text"))[, .(mb = MB_CODE_2021, postcode = POA_CODE_2021, area = fcoalesce(as.numeric(AREA_ALBERS_SQKM), 0))]
lgm <- as.data.table(read_excel(file.path(RAW, X$mb_lga_file), col_types = "text"))[, .(mb = MB_CODE_2021, lga_code = LGA_CODE_2023)]
mb <- Reduce(function(a, b) merge(a, b, by = "mb"), list(poa, lgm, mb))
logf("mesh blocks: %s", comma(nrow(mb)))

# share of each postcode in each council area, by people or by dwellings
# (falls back to land area for postcodes with nobody living in them)
pc_lga <- mb[, .(persons = sum(persons), dwellings = sum(dwellings), area = sum(area)), by = .(postcode, lga_code)]
for (w in c("persons", "dwellings")) {
  pc_lga[, (paste0("w_", w)) := if (sum(get(w)) > 0) get(w) / sum(get(w)) else area / sum(area), by = postcode]
}
pc_lga <- pc_lga[lga_code %chin% inc$lga_code]
lga_pop <- pc_lga[, .(persons = sum(persons), dwellings = sum(dwellings)), by = lga_code]
pc_pop <- mb[, .(persons = sum(persons), dwellings = sum(dwellings)), by = postcode]

# postcode table -> council areas: value columns shared out by the chosen weight
to_lga <- function(d, cols, weight, what) {
  d <- copy(d)[, postcode := sprintf("%04d", as.integer(postcode))]
  m <- merge(d, pc_lga[, c("postcode", "lga_code", paste0("w_", weight)), with = FALSE], by = "postcode", allow.cartesian = TRUE)
  miss <- d[!postcode %chin% pc_lga$postcode]
  if (nrow(miss)) {
    adj(what, "Postcodes with no ABS postal area left out", "PO box, large-volume and unknown postcodes have no mesh blocks, so no council area",
      sprintf("%d postcodes; %s of %s", nrow(miss), pct1(sum(miss[[cols[1]]]) / sum(d[[cols[1]]])), cols[1]))
  }
  m[, (cols) := lapply(.SD, function(v) v * get(paste0("w_", weight))), .SDcols = cols]
  m[, lapply(.SD, sum), by = lga_code, .SDcols = cols]
}

# ---- BITRE: light-vehicle fleet by garaging postcode, 31 January each year ----------
logf("reading BITRE fleet files")
bitre <- rbindlist(lapply(Sys.glob(file.path(RAW, X$bitre_glob)), function(f) {
  d <- fread(f, colClasses = "character")
  d <- d[vehicle_type %chin% unlist(X$bitre_light_types) & motive_power != "-" & grepl("^[0-9]{4}$", garaging_postcode)]
  d[, n := as.numeric(no_vehicles)]
  d[, .(lv = sum(n), bev = sum(n[motive_power == X$bitre_bev_label])), by = .(postcode = garaging_postcode)][, year := as.integer(sub(".*_([0-9]{4})\\.csv$", "\\1", f))][]
}))
YEARS <- sort(unique(bitre$year))
adj(
  "BITRE Road vehicles Australia", "Light vehicles = passenger + light commercial",
  sprintf("Vehicle types %s; BEV = motive power '%s' (fuel-cell cars are a handful). Counted at the garaging postcode", listing(unlist(X$bitre_light_types)), X$bitre_bev_label),
  sprintf("January %d-%d", min(YEARS), max(YEARS))
)
adj(
  "BITRE Road vehicles Australia", "Small counts randomly perturbed by BITRE",
  "BITRE adds small random changes to small non-zero cells to protect privacy; errors are unbiased and wash out in sums",
  "all postcode cells"
)
bitre_wide <- function(d, id) {
  w <- dcast(d, as.formula(paste(id, "~ year")), value.var = c("lv", "bev"), fill = 0)
  setnames(w, sub("^(lv|bev)_", "\\1_", names(w)))
  w
}
b_lga <- rbindlist(lapply(YEARS, function(y) to_lga(bitre[year == y], c("lv", "bev"), "persons", sprintf("BITRE fleet, Jan %d", y))[, year := y]))
b_lga <- bitre_wide(b_lga, "lga_code")

# ---- CER: small-scale solar and batteries by postcode, monthly ----------------------
logf("reading CER postcode data")
read_cer <- function(key) {
  d <- fread(file.path(RAW, X$cer_files[[key]]), colClasses = "character")
  setnames(d, 1, "postcode")
  mcols <- grep("^[A-Z][a-z]{2} [0-9]{4} - ", names(d), value = TRUE)
  hist <- grep("^Historic", names(d), value = TRUE)
  long <- melt(d[, c("postcode", mcols), with = FALSE], id.vars = "postcode", variable.name = "col", value.name = "v")
  # the CER writes numbers with thousands separators ("1,228.135")
  cer_num <- function(x) as.numeric(gsub(",", "", x, fixed = TRUE))
  long[, `:=`(month = format(as.Date(paste0("01 ", substr(col, 1, 8)), "%d %b %Y"), "%Y-%m"), v = cer_num(v), col = NULL)]
  tot <- long[, .(v = sum(v)), by = postcode]
  if (length(hist)) tot[d, v := v + cer_num(get(hist)), on = "postcode"]
  list(month = long[, key := key][], total = tot[, key := key][])
}
cer <- lapply(names(X$cer_files), read_cer)
cer_tot <- dcast(rbindlist(lapply(cer, `[[`, "total")), postcode ~ key, value.var = "v", fill = 0)
cer_mon <- dcast(rbindlist(lapply(cer, `[[`, "month"))[month >= X$series_from], postcode + month ~ key, value.var = "v", fill = 0)
CER_LAST <- max(cer_mon$month)
BAT_FIRST <- min(cer[[which(names(X$cer_files) == "battery_n")]]$month$month)
CER_COLS <- names(X$cer_files)
adj(
  "CER small-scale installations", "Solar = every system with certificates since 2001",
  "Includes new systems, upgrades to existing ones and off-grid systems (CER definition), so counts can exceed homes with solar",
  sprintf("to %s", ym_long(CER_LAST))
)
adj(
  "CER small-scale installations", "Batteries only from July 2025",
  "Batteries became eligible for certificates on 1 July 2025 (Cheaper Home Batteries). Earlier batteries are not in the postcode data",
  sprintf("%s-%s", ym_long(BAT_FIRST), ym_long(CER_LAST))
)
adj(
  "CER small-scale installations", "Latest months are incomplete",
  "Certificates can be created up to 12 months after installation, so recent months grow as later data arrive",
  "last few months"
)
c_lga <- to_lga(cer_tot, CER_COLS, "dwellings", "CER solar + batteries")
c_lga_m <- rbindlist(lapply(split(cer_mon, cer_mon$month), function(d) to_lga(d, CER_COLS, "dwellings", "CER monthly")[, month := d$month[1]]))
.ADJ$rows <- Filter(function(r) r$dataset != "CER monthly", .ADJ$rows) # same postcodes as the totals; logged once

# ---- OpenStreetMap: public charging stations ----------------------------------------
osm_file <- file.path(RAW, X$osm_file)
marker <- file.path(dirname(osm_file), "refresh_requested") # written by update_sources.R
want <- !file.exists(osm_file) || "--refresh-osm" %in% commandArgs(TRUE) || (file.exists(marker) && file.mtime(marker) > file.mtime(osm_file))
if (want) {
  b <- unlist(X$overpass_bbox)
  q <- sprintf('[out:json][timeout:300];nwr["amenity"="charging_station"](%s);out center tags;', paste(b, collapse = ","))
  # public Overpass servers are often busy: retry, then try the next server
  fetch <- function(url) {
    logf("downloading charging stations from OpenStreetMap (%s)", url)
    request(url) |>
      req_user_agent("EV-uptake-by-income (github.com/mhs795/EV-uptake-by-income)") |>
      req_body_form(data = q) |>
      req_timeout(400) |>
      req_retry(max_tries = 3, is_transient = function(r) resp_status(r) %in% c(429, 502, 503, 504), backoff = function(i) 30 * i) |>
      req_perform() |>
      resp_body_raw()
  }
  got <- NULL
  for (u in unlist(X$overpass_urls)) {
    got <- tryCatch(fetch(u), error = function(e) {
      logf("  %s failed: %s", u, conditionMessage(e))
      NULL
    })
    # a reply must be complete JSON with a data timestamp
    if (!is.null(got) && !is.null(tryCatch(fromJSON(rawToChar(got), simplifyVector = FALSE)$osm3s$timestamp_osm_base, error = function(e) NULL))) break
    got <- NULL
  }
  dir.create(dirname(osm_file), showWarnings = FALSE, recursive = TRUE)
  if (!is.null(got)) {
    writeBin(got, osm_file)
  } else if (file.exists(osm_file)) {
    logf("  WARNING: every Overpass server failed; keeping the charging-station snapshot from %s", format(file.mtime(osm_file), "%d %b %Y"))
  } else {
    stop("could not download charging stations from any Overpass server: ", paste(unlist(X$overpass_urls), collapse = ", "))
  }
}
osm <- fromJSON(osm_file, simplifyVector = FALSE)
OSM_DATE <- substr(osm$osm3s$timestamp_osm_base, 1, 10)
tag <- function(e, k) if (is.null(e$tags[[k]])) NA_character_ else e$tags[[k]]
ch <- rbindlist(lapply(osm$elements, function(e) {
  t <- e$tags
  kw <- suppressWarnings(as.numeric(sub("\\s*kW$", "", unlist(t[grepl("output$", names(t))]), ignore.case = TRUE)))
  data.table(
    osm_id = paste0(e$type, "/", e$id), lat = e$lat %||% e$center$lat, lon = e$lon %||% e$center$lon,
    name = fcoalesce(tag(e, "name"), tag(e, "brand"), tag(e, "operator"), ""),
    operator = fcoalesce(tag(e, "operator"), tag(e, "network"), tag(e, "brand"), ""),
    access = fcoalesce(tag(e, "access"), ""), capacity = suppressWarnings(as.integer(tag(e, "capacity"))),
    fast = any(paste0("socket:", unlist(X$fast_sockets)) %in% names(t)) || isTRUE(max(c(kw, 0), na.rm = TRUE) >= X$fast_min_kw)
  )
}))
priv <- ch$access %chin% unlist(X$osm_private_access)
adj("OpenStreetMap charging stations", "Dropped private sites", sprintf("access = %s", listing(unlist(X$osm_private_access))), sprintf("%d of %d sites", sum(priv), nrow(ch)))
ch <- ch[!priv]

sf_use_s2(FALSE)
geo_lga <- st_make_valid(st_read(file.path(OUT, "lga_boundaries.geojson"), quiet = TRUE)[, "lga_code_2023"])
pts <- st_as_sf(ch, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
j <- suppressMessages(st_join(pts, geo_lga, join = st_within))
j <- j[!duplicated(j$osm_id), ]
# the map polygons are generalised, so a few coastal sites fall just outside: use the nearest area within ~5 km
out <- is.na(j$lga_code_2023)
if (any(out)) {
  alb <- function(g) st_transform(g, 3577) # GDA94 Australian Albers, metres
  nn <- suppressMessages(st_nearest_feature(pts[out, ], geo_lga))
  d <- as.numeric(st_distance(alb(pts[out, ]), alb(geo_lga[nn, ]), by_element = TRUE))
  j$lga_code_2023[which(out)[d < 5000]] <- geo_lga$lga_code_2023[nn[d < 5000]]
}
ch[, lga_code := j$lga_code_2023[match(osm_id, j$osm_id)]]
ch <- ch[lga_code %chin% inc$lga_code] # drops sites offshore or outside the council areas kept
geo_poa <- st_make_valid(st_read(file.path(OUT, "vic_postcode_boundaries.geojson"), quiet = TRUE)[, "poa_code_2021"])
jp <- suppressMessages(st_join(st_as_sf(ch, coords = c("lon", "lat"), crs = 4326, remove = FALSE), geo_poa, join = st_within))
ch[, vic_postcode := as.integer(jp$poa_code_2021[match(osm_id, jp$osm_id)])]
ch[, state := inc$state[match(lga_code, inc$lga_code)]]
adj(
  "OpenStreetMap charging stations", "Volunteer-mapped: fast chargers well covered, small AC sites less so",
  sprintf("Snapshot %s. A site is 'fast' if it has a DC plug (%s) or any output of %d kW or more", OSM_DATE, listing(unlist(X$fast_sockets)), X$fast_min_kw),
  sprintf("%s public sites, %s fast", comma(nrow(ch)), comma(sum(ch$fast)))
)
ch_lga <- ch[, .(chg_sites = .N, chg_fast = sum(fast), chg_points = sum(fcoalesce(capacity, 1L))), by = lga_code]
setorder(ch, state, lga_code, name)

# ---- council-area table --------------------------------------------------------------
L <- Reduce(function(a, b) merge(a, b, by = "lga_code", all.x = TRUE), list(inc[, lga_code := as.character(lga_code)], lga_pop, b_lga, c_lga, ch_lga))
num <- setdiff(names(L), c("state", "lga_code", "lga_name"))
for (c in num) set(L, which(is.na(L[[c]])), c, 0)
rnd <- setdiff(num, c("median_income", "earners", "income_group"))
L[, (rnd) := lapply(.SD, function(v) round(v, 1)), .SDcols = rnd]
setorder(L, state, lga_name)
write_out(L, "lga_context.csv")
logf("council areas: %d (%s)", nrow(L), paste(L[, .N, by = state][, paste(state, N)], collapse = ", "))

M <- merge(c_lga_m, inc[, .(lga_code, state)], by = "lga_code")
M[, (CER_COLS) := lapply(.SD, function(v) round(v, 2)), .SDcols = CER_COLS]
setorder(M, state, lga_code, month)
write_out(M[, c("state", "lga_code", "month", CER_COLS), with = FALSE], "solar_battery_lga_month.csv")

# ---- VIC postcodes (no allocation needed: the data are by postcode already) ----------
pv <- fread(file.path(OUT, "postcode_income.csv"))[, .(postcode = sprintf("%04d", postcode))]
bv <- bitre_wide(bitre, "postcode")
P <- Reduce(function(a, b) merge(a, b, by = "postcode", all.x = TRUE), list(pv, pc_pop, bv, cer_tot))
P <- merge(P, ch[!is.na(vic_postcode), .(chg_sites = .N, chg_fast = sum(fast), chg_points = sum(fcoalesce(capacity, 1L))), by = .(postcode = sprintf("%04d", vic_postcode))], by = "postcode", all.x = TRUE)
for (c in setdiff(names(P), "postcode")) set(P, which(is.na(P[[c]])), c, 0)
P[, postcode := as.integer(postcode)]
setorder(P, postcode)
write_out(P, "vic_postcode_context.csv")
write_out(ch, "chargers.csv")
write_out(data.table(
  item = c("bitre_years", "cer_last_month", "battery_first_month", "osm_date"),
  value = c(paste(YEARS, collapse = ","), CER_LAST, BAT_FIRST, OSM_DATE)
), "context_dates.csv")

# append to the data-adjustments log written by build_data.R (replacing any earlier context rows)
a_old <- fread(file.path(OUT, "adjustments.csv"))
a_new <- adjustments()
a_old <- a_old[!dataset %chin% unique(a_new$dataset)]
write_out(rbind(a_old, a_new), "adjustments.csv")
logf("chargers: %s public sites (%s fast); solar/battery data to %s; BITRE %d-%d", comma(nrow(ch)), comma(sum(ch$fast)), CER_LAST, min(YEARS), max(YEARS))
