# Regenerate tests/testthat/fixtures/eer_bad_hour.rds
#
# The fixture is the ECCC EER Canada smoke shapefile for 2026-08-09 02:00 UTC,
# filtered down to the two contour bands (500 and 5 ug/m3) that reproduce the
# GEOS "unable to assign free hole to a shell" failure that the single n-ary
# `sf::st_difference()` hit in `remove_polygon_overlap()` on that hour. ECCC
# only archives ~8 days of runs, so the fixture is saved rather than
# re-downloaded in tests; run this script to rebuild it from the live archive
# before the run expires.
#
# The model run serving 02:00 UTC on 2026-08-09 is the 00:00 UTC run.
zip_url <- "https://eer.cmc.ec.gc.ca/mandats/AutoSim/Fire/00UTC/Canada/20260809.0000/shp/shp_Canada.zip"
extract_dir <- tempfile("eer_")
dir.create(extract_dir)
zip_path <- file.path(extract_dir, "shp_Canada.zip")
utils::download.file(zip_url, zip_path, mode = "wb", quiet = TRUE)
utils::unzip(zip_path, exdir = extract_dir)

shp_path <- list.files(
  extract_dir,
  pattern = "shp_Canada_20260809-0200.*\\.shp$",
  recursive = TRUE,
  full.names = TRUE
)

eer_bad_hour <- sf::read_sf(shp_path) |>
  dplyr::filter(.data$Interval %in% c(500, 5))

saveRDS(
  eer_bad_hour,
  "tests/testthat/fixtures/eer_bad_hour.rds",
  compress = "xz"
)
