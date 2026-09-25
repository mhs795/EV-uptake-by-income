#!/usr/bin/env Rscript
# Open the EV × Income Shiny dashboard in your browser (local only).
#   Rscript run_dashboard.R            # default port 8050
#   Rscript run_dashboard.R 8080       # another port
# Needs processed/dashboard.rds — run `Rscript run_all.R` first if it is missing.
f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
if (!length(f)) f <- sys.frames()[[1]]$ofile  # RStudio's Source button
root <- if (length(f)) dirname(normalizePath(f)) else getwd()
Sys.setenv(EV_ROOT = root)
if (!file.exists(file.path(root, "processed", "dashboard.rds")))
  stop("processed/dashboard.rds not found — run `Rscript run_all.R` (or `--only dashboard`) first")
port <- as.integer(commandArgs(trailingOnly = TRUE)[1])
if (is.na(port)) port <- 8050L
shiny::runApp(file.path(root, "app"), host = "127.0.0.1", port = port, launch.browser = TRUE)
