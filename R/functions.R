make_leaflet_map <- function(marker_data = NULL) {
  map <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap)

  if (!is.null(marker_data)) {
    map <- base_map |>
      leaflet::addMarkers(
        data = marker_data,
        lng = ~lng,
        lat = ~lat,
        icon = ~leaflet::icons(icon_url, iconWidth = 32, iconHeight = 32)
      )
  }

  return(map)
}

# Download and read in most recent AQmap obs datafile
load_recent_aqmap_data <- function(data_dir = "../data") {
  aqmap_url <- "https://aqmap.ca/aqmap"
  file_name <- "aqmap_most_recent_obs.Rds"
  local_path <- file.path(data_dir, file_name)
  desired_cols <- c(
    site_id = "sensor_index",
    name = "monitor",
    "network",
    "lat",
    "lng",
    "prov_terr",
    date_last_obs = "date",
    "pm25_1hr",
    icon_url = "icon_url_1hr"
  )

  # Download .rds file
  aqmap_url |>
    file.path("data", file_name) |>
    download.file(local_path, mode = "wb")

  # Load and cleanup
  readRDS(local_path) |>
    dplyr::select(dplyr::any_of(desired_cols)) |>
    dplyr::filter(!is.na(pm25_1hr)) |>
    dplyr::mutate(icon_url = file.path(aqmap_url, icon_url))
}
