#!/usr/bin/env Rscript
# Open the EV × Income Shiny dashboard in your browser (local only).
# Installs any missing R packages first, so on a new PC this is the only script you need to run.
#   Rscript run_dashboard.R            # default port 8050
#   Rscript run_dashboard.R 8080       # another port
# Needs processed/dashboard.rds — run `Rscript run_all.R` first if it is missing.
f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
if (!length(f)) f <- sys.frames()[[1]]$ofile  # RStudio's Source button
root <- if (length(f)) dirname(normalizePath(f)) else getwd()
Sys.setenv(EV_ROOT = root)
# first run on a new PC: install any missing R packages (needs internet; a few minutes)
source(file.path(root, "R", "install_packages.R"), local = new.env())
if (!file.exists(file.path(root, "processed", "dashboard.rds")))
  stop("processed/dashboard.rds not found — run `Rscript run_all.R` (or `--only dashboard`) first")
port <- as.integer(commandArgs(trailingOnly = TRUE)[1])
if (is.na(port)) port <- 8050L
# port taken (usually the dashboard is already open, e.g. in RStudio): use a free one instead
srv <- tryCatch(httpuv::startServer("127.0.0.1", port, list()), error = function(e) NULL)
if (is.null(srv)) {
  new <- httpuv::randomPort()
  message(sprintf("Port %d is in use (is the dashboard already open?) - using port %d", port, new))
  port <- new
} else httpuv::stopServer(srv)
shiny::runApp(file.path(root, "app"), host = "127.0.0.1", port = port, launch.browser = TRUE)
