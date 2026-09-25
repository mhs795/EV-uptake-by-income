# Install the R packages this project uses (only the ones you don't have).
# On Linux, binaries come from Posit Package Manager, which is much faster than
# building from source. sf needs the GDAL/GEOS/PROJ system libraries
# (Ubuntu: sudo apt install libgdal-dev libgeos-dev libproj-dev libudunits2-dev).
pkgs <- c("data.table", "readxl", "yaml", "jsonlite", "httr2", "sf", "openxlsx2",
          "shiny", "bslib", "leaflet", "plotly", "htmltools")
missing <- setdiff(pkgs, rownames(installed.packages()))
if (!length(missing)) { cat("All packages already installed.\n"); quit(status = 0) }
repo <- "https://cloud.r-project.org"
if (Sys.info()[["sysname"]] == "Linux" && file.exists("/etc/os-release")) {
  os <- readLines("/etc/os-release")
  code <- sub("^VERSION_CODENAME=", "", grep("^VERSION_CODENAME=", os, value = TRUE))
  if (length(code) && nzchar(code)) {
    repo <- sprintf("https://packagemanager.posit.co/cran/__linux__/%s/latest", code)
    options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(), paste(getRversion(), R.version$platform, R.version$arch, R.version$os)))
  }
}
cat("Installing:", missing, "\nfrom", repo, "\n")
install.packages(missing, repos = repo, lib = .libPaths()[1], Ncpus = max(1L, parallel::detectCores() - 1L))
still <- setdiff(pkgs, rownames(installed.packages()))
if (length(still)) stop("Could not install: ", paste(still, collapse = ", "))
