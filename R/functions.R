make_leaflet_map <- function(marker_data = NULL) {
  map <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap)

  if (!is.null(marker_data)) {
    map <- map |>
      leaflet::addMarkers(
        data = marker_data,
        lng = ~lng,
        lat = ~lat,
        icon = ~ leaflet::icons(icon_url, iconWidth = 32, iconHeight = 32)
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

  # Determine time since local file was updated, 
  # (relative to when file should have been last updated)
  newest_file <- lubridate::now(tz = "UTC") |> 
    lubridate::floor_date("10 mins")
  local_file_age <- get_file_age(local_path, since = newest_file)
  
  # Download .rds file if needed
  if (local_file_age > "10 mins"){
    aqmap_url |>
      file.path("data", file_name) |>
      download.file(local_path, mode = "wb")
  }

  # Load and cleanup
  readRDS(local_path) |>
    dplyr::select(dplyr::any_of(desired_cols)) |>
    # dplyr::filter(!is.na(pm25_1hr)) |>
    dplyr::mutate(icon_url = file.path(aqmap_url, icon_url))
}

get_file_age <- function(local_path, since = Sys.time()) {
  if (!file.exists(local_path)) {
    return(as.difftime(Inf, units = "days"))
  }
  last_update_time <- file.info(local_path)$mtime
  
  since - last_update_time
}
