# Download the public raw data listed in raw_data/**/urls.txt (~1.8 GB).
# Files already on disk are skipped, so it is safe to re-run. To refresh a
# dataset, delete its files (or update its urls.txt with newer resources) and re-run.
source(if (file.exists("R/common.R")) "R/common.R" else file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), "common.R"))
options(timeout = 3600)

# folder -> urls file; QLD lines are "<name> <url>" and are saved as qld_<name>.csv
lists <- list(list(dir = "nsw", urls = "nsw/urls.txt"),
              list(dir = "nsw/snapshot", urls = "nsw/snapshot_urls.txt"),
              list(dir = "nsw/age", urls = "nsw/age/urls.txt"),
              list(dir = "qld", urls = "qld/urls.txt", named = "qld_%s.csv"),
              list(dir = "vic", urls = "vic/urls.txt"),
              list(dir = "income", urls = "income/urls.txt",
                   names = c("abs_pia_table1.xlsx", "ts24individual08medianaveragetaxableincomestatepostcode.xlsx",
                             "ts24individual06taxablestatusstatesa4postcode.xlsx")))
n_new <- 0
for (l in lists) {
  dir.create(file.path(RAW, l$dir), showWarnings = FALSE, recursive = TRUE)
  lines <- trimws(readLines(file.path(RAW, l$urls), warn = FALSE)); lines <- lines[nzchar(lines)]
  for (i in seq_along(lines)) {
    parts <- strsplit(lines[i], "\\s+")[[1]]
    url <- tail(parts, 1)
    fname <- if (!is.null(l$named)) sprintf(l$named, parts[1]) else if (!is.null(l$names)) l$names[i] else URLdecode(basename(sub("\\?.*", "", url)))
    dest <- file.path(RAW, l$dir, fname)
    if (file.exists(dest) && file.size(dest) > 0) next
    logf("downloading %s", file.path(l$dir, fname))
    tmp <- paste0(dest, ".part")
    ok <- tryCatch(download.file(url, tmp, mode = "wb", quiet = TRUE) == 0, error = function(e) { message(conditionMessage(e)); FALSE })
    if (!ok) stop("download failed: ", url)
    file.rename(tmp, dest); n_new <- n_new + 1
  }
}
logf("raw data ready (%d new files); fuel prices are in raw_data/fuel (kept in the repo)", n_new)
