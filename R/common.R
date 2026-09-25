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
  if (length(f)) return(normalizePath(file.path(dirname(f), "..")))
  if (!is.null(sys.frames()[[1]]$ofile)) return(normalizePath(file.path(dirname(sys.frames()[[1]]$ofile), "..")))
  normalizePath(".")
}
HERE <- Sys.getenv("EV_ROOT", find_root())
CFG  <- yaml::read_yaml(file.path(HERE, "config.yaml"))
RAW  <- file.path(HERE, CFG$paths$raw)
OUT  <- file.path(HERE, Sys.getenv("EV_PROCESSED", CFG$paths$processed))
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

FUEL_GROUPS <- c("bev", "phev", "other")

logf <- function(...) { cat(sprintf(...), "\n", sep = ""); flush.console() }
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
ym_to_int <- function(x) { y <- as.integer(substr(x, 1, 4)); m <- as.integer(substr(x, 6, 7)); y * 12L + m - 1L }
int_to_ym <- function(i) sprintf("%04d-%02d", i %/% 12L, i %% 12L + 1L)
ym_seq <- function(a, b) int_to_ym(seq(ym_to_int(a), ym_to_int(b)))

write_out <- function(dt, name) fwrite(dt, file.path(OUT, name))
