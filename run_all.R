#!/usr/bin/env Rscript
# EV uptake by regional income — run the whole project.
#
#   Rscript run_all.R                 # every step, in order
#   Rscript run_all.R --from workbook # start at a later step
#   Rscript run_all.R --only data     # run one step
#
# Steps
#   install     install the R packages the project uses (skips ones you have)
#   download    fetch the public raw data (~1.8 GB) into raw_data/ (skips files you have)
#   data        raw_data/ -> processed/*.csv            (R/build_data.R, ~5 min)
#   boundaries  ABS boundaries + VIC suburb names       (R/fetch_boundaries.R)
#   workbook    -> EV_uptake_by_income.xlsx             (R/build_workbook.R)
#   dashboard   -> processed/dashboard.rds for the app  (R/dashboard_data.R)
#
# Then open the dashboard with:  Rscript run_dashboard.R
# All settings are in config.yaml.

steps <- c(install = "R/install_packages.R", download = "R/download_raw.R", data = "R/build_data.R",
           boundaries = "R/fetch_boundaries.R", workbook = "R/build_workbook.R", dashboard = "R/dashboard_data.R")

args <- commandArgs(trailingOnly = TRUE)
pick <- function(flag) { i <- match(flag, args); if (is.na(i)) NULL else args[i + 1] }
run <- names(steps)
if (!is.null(f <- pick("--from"))) run <- run[seq(match(f, run), length(run))]
if (!is.null(o <- pick("--only"))) run <- o
bad <- setdiff(run, names(steps))
if (length(bad)) stop("Unknown step: ", paste(bad, collapse = ", "), ". Steps are: ", paste(names(steps), collapse = ", "))

# Run from the project folder, whatever the caller's working directory
f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
if (!length(f)) f <- sys.frames()[[1]]$ofile  # RStudio's Source button
if (length(f)) setwd(dirname(normalizePath(f)))

for (s in run) {
  cat(sprintf("\n==== %s  (%s) ====\n", s, steps[[s]]))
  t0 <- Sys.time()
  # each step runs in a fresh R process: clean memory, same as running it by hand
  status <- system2(shQuote(file.path(R.home("bin"), "Rscript")), shQuote(steps[[s]]))  # quoted: R on Windows lives in "Program Files"
  if (status != 0) stop(sprintf("step '%s' failed (exit %d)", s, status))
  cat(sprintf("---- %s done in %.0f s\n", s, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
cat("\nAll done. Workbook: EV_uptake_by_income.xlsx   Dashboard: Rscript run_dashboard.R\n")
