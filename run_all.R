#!/usr/bin/env Rscript
# EV uptake by regional income — run the whole project.
#
#   Rscript run_all.R                 # every step, in order
#   Rscript run_all.R --from workbook # start at a later step
#   Rscript run_all.R --only data     # run one step
#   Rscript run_all.R --from update   # fetch the latest data and rebuild (the dashboard's "Update data" button)
#
# Steps
#   install     install the R packages the project uses (skips ones you have)
#   update      find the latest files on each open-data portal; mark changed ones for download
#   download    fetch the public raw data (~1.9 GB) into raw_data/ (skips files you have)
#   data        raw_data/ -> processed/*.csv            (R/build_data.R, ~5 min)
#   boundaries  ABS boundaries + VIC suburb names       (R/fetch_boundaries.R)
#   context     all states: BEV fleet, solar, batteries, charging stations (R/build_context.R)
#   workbook    -> EV_uptake_by_income.xlsx             (R/build_workbook.R)
#   dashboard   -> processed/dashboard.rds for the app  (R/dashboard_data.R)
#
# Then open the dashboard with:  Rscript run_dashboard.R
# All settings are in config.yaml.

steps <- c(
  install = "R/install_packages.R", update = "R/update_sources.R", download = "R/download_raw.R", data = "R/build_data.R",
  boundaries = "R/fetch_boundaries.R", context = "R/build_context.R", workbook = "R/build_workbook.R", dashboard = "R/dashboard_data.R"
)

args <- commandArgs(trailingOnly = TRUE)
pick <- function(flag) {
  i <- match(flag, args)
  if (is.na(i)) NULL else args[i + 1]
}
run <- names(steps)
if (!is.null(f <- pick("--from"))) run <- run[seq(match(f, run), length(run))]
if (!is.null(o <- pick("--only"))) run <- o
bad <- setdiff(run, names(steps))
if (length(bad)) stop("Unknown step: ", paste(bad, collapse = ", "), ". Steps are: ", paste(names(steps), collapse = ", "))

# Run from the project folder, whatever the caller's working directory
f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
if (!length(f)) f <- sys.frames()[[1]]$ofile # RStudio's Source button
if (length(f)) setwd(dirname(normalizePath(f)))

# --status FILE: write "running", then "ok" or "failed: <reason>" (the dashboard polls it)
status_file <- pick("--status")
set_status <- function(x) if (!is.null(status_file)) writeLines(x, status_file)
set_status("running")
tryCatch(
  {
    for (s in run) {
      cat(sprintf("\n==== %s  (%s) ====\n", s, steps[[s]]))
      t0 <- Sys.time()
      # each step runs in a fresh R process: clean memory, same as running it by hand
      status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(steps[[s]])) # system2 quotes the command itself (R on Windows is in "Program Files")
      if (status != 0) stop(sprintf("step '%s' failed (exit %d)", s, status))
      cat(sprintf("---- %s done in %.0f s\n", s, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    }
  },
  error = function(e) {
    set_status(paste("failed:", conditionMessage(e)))
    stop(e)
  }
)
set_status("ok")
cat("\nAll done. Workbook: EV_uptake_by_income.xlsx   Dashboard: Rscript run_dashboard.R\n")
