# Find the latest public data files and mark changed ones for download.
#
# Lists every file on the NSW, QLD, VIC and data.gov.au (BITRE) open-data portals,
# rewrites raw_data/**/urls.txt, and lists in raw_data/refresh.txt any local file the
# portal has updated since it was downloaded (current-year files are replaced in place
# each month). The CER postcode files are always listed, and the OpenStreetMap charging
# stations are always re-fetched by build_context.R. Nothing is deleted here: the
# download step swaps in each new file only once it has arrived. Monthly fuel prices
# are copied from the au_fuel_prices project when it is on this computer.
#
# Run:  Rscript R/update_sources.R     (or the dashboard's "Update data" button)
# common.R sits next to this script: found via Rscript's --file, or source()'s ofile (RStudio's Source button)
source(file.path(local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (!length(f)) f <- sys.frames()[[1]]$ofile
  if (length(f)) dirname(normalizePath(f)) else "R"
}), "common.R"))
suppressPackageStartupMessages(library(httr2))
U <- CFG$update
UA <- "EV-uptake-by-income (github.com/mhs795/EV-uptake-by-income)"

ckan <- function(portal, action, ...) {
  resp <- request(sprintf("%s/api/3/action/%s", portal, action)) |>
    req_url_query(...) |>
    req_user_agent(UA) |>
    req_timeout(120) |>
    req_retry(max_tries = 3) |>
    req_perform()
  resp_body_json(resp)$result
}
# one row per file; matched on the file's name and URL, never on its declared format
# (portals leave the format blank on some files)
resources <- function(pkg) {
  rbindlist(lapply(pkg$resources, function(r) {
    data.table(name = r$name %||% "", url = r$url %||% "", modified = r$last_modified %||% r$metadata_modified %||% r$created %||% NA_character_)
  }))
}
`%||%` <- function(a, b) if (is.null(a)) b else a
year_of <- function(x) as.integer(sub(".*?((19|20)[0-9]{2}).*", "\\1", x))

refresh <- character()
# write a urls.txt and mark local copies the portal has changed since they were downloaded
publish <- function(dir, urls_file, d, fname) {
  writeLines(d$line, file.path(RAW, urls_file))
  for (i in seq_len(nrow(d))) {
    f <- file.path(RAW, dir, fname[i])
    if (!file.exists(f) || is.na(d$modified[i])) next
    remote <- as.POSIXct(sub("\\..*", "", d$modified[i]), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    if (!is.na(remote) && remote > file.mtime(f)) {
      logf("  updated on the portal: %s", file.path(dir, fname[i]))
      refresh <<- c(refresh, file.path(dir, fname[i]))
    }
  }
  logf("%s: %d files listed", urls_file, nrow(d))
}

# ---- NSW: yearly transaction and snapshot files --------------------------------------
logf("NSW (TfNSW open data)")
r <- resources(ckan(U$nsw$portal, "package_show", id = U$nsw$package))
for (k in list(
  list(pat = "Vehicle Registration Transactions", file = "nsw/urls.txt", dir = "nsw"),
  list(pat = "Vehicle Registrations Snapshot", file = "nsw/snapshot_urls.txt", dir = "nsw/snapshot")
)) {
  d <- r[grepl(k$pat, name, fixed = TRUE) & grepl("\\.zip$", url)][, year := year_of(name)][year >= U$since_year][order(-year)]
  d[, line := url]
  publish(k$dir, k$file, d, basename(d$url))
}
# the age-of-fleet file is pinned to the calibration month in config (nsw.age_snapshot_file)

# ---- QLD: unit records, two files per year ---------------------------------------------
logf("QLD (data.qld.gov.au)")
r <- resources(ckan(U$qld$portal, "package_show", id = U$qld$package))
d <- r[grepl("Vehicle Regi.*Details", name) & grepl("\\.csv$", url)][, year := year_of(name)][year >= U$since_year]
d[, part := seq_len(.N), by = year] # files numbered in portal order within each year
setorder(d, -year, part)
d[, line := sprintf("%d_%d %s", year, part, url)]
publish("qld", "qld/urls.txt", d, sprintf("qld_%d_%d.csv", d$year, d$part))

# ---- VIC: quarterly whole-fleet snapshots by postcode ---------------------------------
logf("VIC (DTP open data)")
r <- resources(ckan(U$vic$portal, "package_show", id = U$vic$package))
d <- r[grepl("snapshot_by_postcode_q[1-4]_[0-9]{4}\\.csv$", url)][, line := url]
publish("vic", "vic/urls.txt", d, basename(d$url))

# ---- BITRE: Road vehicles Australia, one dataset per January ---------------------------
logf("BITRE (data.gov.au)")
p <- ckan(U$bitre$portal, "package_search", q = U$bitre$search, rows = 50)$results
d <- rbindlist(lapply(p, function(pk) {
  if (!grepl(paste0("^", U$bitre$search, "-[0-9]{4}$"), pk$name)) {
    return(NULL)
  }
  rr <- resources(pk)[grepl(U$bitre$resource_pattern, url, fixed = TRUE)]
  if (nrow(rr)) rr[1][, year := year_of(pk$name)] else NULL
}))
setorder(d, year)
d[, line := sprintf("%d %s", year, url)]
publish("bitre", "bitre/urls.txt", d, sprintf("bitre_poagar_mtvpwr_%d.csv", d$year))

# ---- always refreshed: CER postcode files (same URLs each month) ---------------------------
refresh <- c(refresh, file.path("cer", basename(Sys.glob(file.path(RAW, "cer", "cer_*.csv")))))
writeLines(unique(refresh), file.path(RAW, "refresh.txt"))
# OpenStreetMap: build_context.R re-fetches it when this marker is newer than the saved snapshot
dir.create(file.path(RAW, "osm"), showWarnings = FALSE)
writeLines(format(Sys.time()), file.path(RAW, "osm", "refresh_requested"))
logf("CER solar/battery files will be downloaded fresh; OpenStreetMap chargers re-fetched in the context step")

# ---- fuel prices from the au_fuel_prices project --------------------------------------
src <- path.expand(U$fuel_source_dir)
for (f in c("retail_monthly.csv", "tgp_monthly.csv")) {
  s <- file.path(src, f)
  dst <- file.path(HERE, CFG$fuel[[sub("_monthly.csv", "_file", f)]])
  if (file.exists(s) && (!file.exists(dst) || file.mtime(s) > file.mtime(dst))) {
    file.copy(s, dst, overwrite = TRUE, copy.date = TRUE)
    logf("fuel prices: copied %s from %s", f, src)
  }
}
if (!dir.exists(src)) logf("fuel prices: %s not found; keeping the copies in raw_data/fuel", src)
logf("sources checked; %d local files marked for fresh download", length(unique(refresh)))
