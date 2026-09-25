# Download the public raw data listed in raw_data/**/urls.txt (~1.9 GB).
# Files already on disk are skipped, so it is safe to re-run. Files listed in
# raw_data/refresh.txt (written by update_sources.R) are downloaded again; the old
# copy is replaced only once the new one has arrived, so a source that is down
# leaves the previous data in place (with a warning) instead of breaking the build.
# common.R sits next to this script: found via Rscript's --file, or source()'s ofile (RStudio's Source button)
source(file.path(local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (!length(f)) f <- sys.frames()[[1]]$ofile
  if (length(f)) dirname(normalizePath(f)) else "R"
}), "common.R"))
options(timeout = 3600)

# folder -> urls file; QLD lines are "<name> <url>" and are saved as qld_<name>.csv
lists <- list(
  list(dir = "nsw", urls = "nsw/urls.txt"),
  list(dir = "nsw/snapshot", urls = "nsw/snapshot_urls.txt"),
  list(dir = "nsw/age", urls = "nsw/age/urls.txt"),
  list(dir = "qld", urls = "qld/urls.txt", named = "qld_%s.csv"),
  list(dir = "vic", urls = "vic/urls.txt"),
  list(
    dir = "income", urls = "income/urls.txt",
    names = c(
      "abs_pia_table1.xlsx", "ts24individual08medianaveragetaxableincomestatepostcode.xlsx",
      "ts24individual06taxablestatusstatesa4postcode.xlsx"
    )
  ),
  # ABS mesh blocks -> postcodes and council areas, with Census 2021 people and dwellings
  list(dir = "abs_mb", urls = "abs_mb/urls.txt", names = c("POA_2021_AUST.xlsx", "LGA_2023_AUST.xlsx", "mesh_block_counts_2021.xlsx")),
  # BITRE Road vehicles Australia (January each year): fleet by garaging postcode and motive power, all states
  list(dir = "bitre", urls = "bitre/urls.txt", named = "bitre_poagar_mtvpwr_%s.csv"),
  # CER small-scale installations by postcode (solar PV, batteries); monthly updates reuse the same URL,
  # so update_sources.R always lists them in refresh.txt
  list(dir = "cer", urls = "cer/urls.txt", named = "cer_%s.csv")
)
n_new <- 0
refresh_file <- file.path(RAW, "refresh.txt")
refresh <- if (file.exists(refresh_file)) readLines(refresh_file, warn = FALSE) else character()
kept <- character()
for (l in lists) {
  dir.create(file.path(RAW, l$dir), showWarnings = FALSE, recursive = TRUE)
  lines <- trimws(readLines(file.path(RAW, l$urls), warn = FALSE))
  lines <- lines[nzchar(lines)]
  for (i in seq_along(lines)) {
    parts <- strsplit(lines[i], "\\s+")[[1]]
    url <- tail(parts, 1)
    fname <- if (!is.null(l$named)) sprintf(l$named, parts[1]) else if (!is.null(l$names)) l$names[i] else URLdecode(basename(sub("\\?.*", "", url)))
    dest <- file.path(RAW, l$dir, fname)
    have <- file.exists(dest) && file.size(dest) > 0
    again <- file.path(l$dir, fname) %in% refresh
    if (have && !again) next
    logf("%s %s", if (have) "refreshing" else "downloading", file.path(l$dir, fname))
    tmp <- paste0(dest, ".part")
    ok <- tryCatch(download.file(url, tmp, mode = "wb", quiet = TRUE) == 0, error = function(e) {
      message(conditionMessage(e))
      FALSE
    })
    if (!ok) {
      unlink(tmp)
      if (!have) stop("download failed: ", url)
      logf("  WARNING: could not refresh %s; keeping the copy from %s", file.path(l$dir, fname), format(file.mtime(dest), "%d %b %Y"))
      kept <- c(kept, file.path(l$dir, fname))
      next
    }
    file.rename(tmp, dest)
    n_new <- n_new + 1
  }
}
# files that failed stay listed, so the next run tries them again
if (file.exists(refresh_file)) {
  if (length(kept)) writeLines(kept, refresh_file) else invisible(file.remove(refresh_file))
}
logf("raw data ready (%d new files%s); fuel prices are in raw_data/fuel (kept in the repo)", n_new, if (length(kept)) sprintf(", %d kept from before", length(kept)) else "")
