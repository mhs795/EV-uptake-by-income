# Download generalised boundaries from the ABS ArcGIS REST service:
#   processed/lga_boundaries.geojson           every council area in Australia (ASGS 2023)
#   processed/vic_postcode_boundaries.geojson  VIC postal areas (ASGS 2021)
#   processed/australia_states.geojson         all states, for the map base layer
#   processed/vic_postcode_suburbs.csv         each VIC postcode named by its ABS suburbs
#
# Run:  Rscript R/fetch_boundaries.R
# common.R sits next to this script: found via Rscript's --file, or source()'s ofile (RStudio's Source button)
source(file.path(local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (!length(f)) f <- sys.frames()[[1]]$ofile
  if (length(f)) dirname(normalizePath(f)) else "R"
}), "common.R"))
suppressPackageStartupMessages({
  library(jsonlite)
  library(httr2)
  library(sf)
})
M <- CFG$map

# Page through an ArcGIS query endpoint and return a GeoJSON FeatureCollection (list)
query <- function(url, where, fields, offset_deg) {
  feats <- list()
  start <- 0
  repeat {
    req <- request(url) |>
      req_url_query(
        where = where, outFields = fields, returnGeometry = "true", outSR = "4326",
        f = "geojson", maxAllowableOffset = offset_deg, geometryPrecision = M$coord_decimals,
        resultOffset = start, orderByFields = "objectid"
      ) |>
      req_timeout(300)
    page <- resp_body_json(req_perform(req), simplifyVector = FALSE)
    got <- page$features
    feats <- c(feats, got)
    more <- isTRUE(page$exceededTransferLimit) || isTRUE(page$properties$exceededTransferLimit)
    if (!length(got) || !more) break
    start <- start + length(got)
  }
  list(type = "FeatureCollection", features = Filter(function(f) !is.null(f$geometry), feats))
}
save_geojson <- function(fc, name) writeLines(toJSON(fc, auto_unbox = TRUE, digits = NA), file.path(OUT, name))

# Name each VIC postcode by the ABS suburbs/localities (SAL 2021) whose
# representative point falls inside it, smallest (most specific) locality first
postcode_suburbs <- function(poa_file) {
  pts <- query(M$sal_point_service, "state_code_2021 = '2'", "sal_name_2021,area_albers_sqkm", 0)
  p <- st_read(toJSON(pts, auto_unbox = TRUE, digits = NA), quiet = TRUE)
  p <- suppressWarnings(st_cast(p, "POINT"))
  p <- p[!duplicated(p$sal_name_2021), ]
  sf_use_s2(FALSE)
  # generalised polygons can self-intersect; repair before the spatial join
  poa <- st_make_valid(st_read(file.path(OUT, poa_file), quiet = TRUE))
  j <- suppressMessages(st_join(p, poa["poa_code_2021"], join = st_within))
  d <- as.data.table(st_drop_geometry(j))[!is.na(poa_code_2021)]
  d[, name := sub(" \\(Vic\\.\\)$", "", sal_name_2021)]
  k <- M$suburb_names_per_postcode
  out <- d[order(area_albers_sqkm), .(suburbs = paste0(paste(head(name, k), collapse = ", "), if (.N > k) paste0(" +", .N - k) else "")),
    by = .(postcode = as.integer(poa_code_2021))
  ]
  setorder(out, postcode)
  write_out(out, "vic_postcode_suburbs.csv")
  logf("VIC postcodes named: %d", nrow(out))
}

lga <- query(M$lga_service, "state_code_2021 IN ('1','2','3','4','5','6','7','8')", "lga_code_2023,lga_name_2023,state_code_2021", M$lga_offset_deg)
save_geojson(lga, "lga_boundaries.geojson")
logf("LGA features: %d", length(lga$features))

v <- CFG$vic
poa <- query(
  M$poa_service, sprintf("poa_code_2021 >= '%d' AND poa_code_2021 <= '%d'", v$postcode_min, v$postcode_max),
  "poa_code_2021", M$poa_offset_deg
)
save_geojson(poa, "vic_postcode_boundaries.geojson")
logf("VIC postcode features: %d", length(poa$features))
postcode_suburbs("vic_postcode_boundaries.geojson")

ste <- query(M$ste_service, "state_code_2021 IN ('1','2','3','4','5','6','7','8')", "state_code_2021,state_name_2021", M$ste_offset_deg)
ste$features <- lapply(ste$features, function(f) {
  f$properties <- list(name = f$properties$state_name_2021)
  f
})
save_geojson(ste, "australia_states.geojson")
logf("State features: %d", length(ste$features))
