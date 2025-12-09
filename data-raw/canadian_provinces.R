prov_order <- c(
  BC = "British Columbia",
  AB = "Alberta",
  SK = "Saskatchewan",
  MB = "Manitoba",
  ON = "Ontario",
  QC = "Québec",
  NB = "New Brunswick",
  NS = "Nova Scotia",
  PE = "Prince Edward Island",
  NL = "Newfoundland and Labrador",
  YT = "Yukon",
  NT = "Northwest Territories",
  NU = "Nunavut"
)

bbox <- osmdata::getbb("Canada", format_out = "sf_polygon")

osm_results <- osmdata::opq(bbox = bbox, timeout = 600) |>
  osmdata::add_osm_feature(key = "type", value = "boundary") |>
  osmdata::add_osm_feature(key = "boundary", value = "administrative") |>
  osmdata::add_osm_feature(key = "border_type", value = "province") |>
  osmdata::add_osm_feature(key = "admin_level", value = "4") |>
  osmdata::osmdata_sf() |>
  osmdata::unique_osmdata()
osm_results <- osm_results$osm_multipolygons

canadian_provinces <- osm_results |>
  dplyr::select("osm_id", "name", "geometry") |>
  dplyr::mutate(
    name = .data$name |>
      handyr::swap(
        "New Brunswick / Nouveau-Brunswick",
        with = "New Brunswick"
      ) |>
      handyr::swap("ᓄᓇᕗᑦ Nunavut", with = "Nunavut") |>
      factor(levels = prov_order),
    abbr = .data$name |>
      factor(levels = prov_order, labels = names(prov_order)),
    type = ifelse(.data$name %in% prov_order[1:10], "province", "territory") |>
      factor(levels = c("province", "territory"))
  ) |>
  dplyr::arrange(.data$name) |>
  withr::with_package(package = "sf")

row.names(canadian_provinces) <- NULL

# Union all provinces
for (i in 1:nrow(canadian_provinces)) {
  canadian_provinces$geometry[i] <- canadian_provinces$geometry[i] |>
    sf::st_union()
}

# Smooth edges for smaller file size
canadian_provinces <- canadian_provinces |>
  rmapshaper::ms_simplify()

# Write out data
usethis::use_data(canadian_provinces, overwrite = TRUE, compress = "xz")

# write out example to geojson as well
geojson_path <- "inst/extdata/example.geojson"
file.remove(geojson_path) |> invisible() |> suppressWarnings()
canadian_provinces |>
  dplyr::filter(.data$abbr == "PE") |>
  sf::st_write(geojson_path, driver = "GeoJSON")
