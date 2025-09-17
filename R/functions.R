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

load_aqmap_plot_data <- function(
  network,
  site_id,
  data_dir = "../data/plotting/"
) {
  stopifnot(length(network) == 1, is.character(network))
  stopifnot(length(site_id) == 1, is.character(site_id) | is.numeric(site_id))

  aqmap_url <- "https://aqmap.ca/aqmap"
  plot_file_template <- "%s_recent_hourly.csv"

  # Handle aliases for network
  network <- translate_network(network)

  # Build desired file url
  site_id <- ifelse(network == "agency", site_id, paste0("sensor_", site_id))
  file_name <- sprintf(plot_file_template, site_id)

  # Build source url and destination path
  source_url <- aqmap_url |>
    file.path("data/plotting/", network, file_name)

  data.table::fread(source_url)
}

get_file_age <- function(local_path, since = Sys.time()) {
  if (!file.exists(local_path)) {
    return(as.difftime(Inf, units = "days"))
  }
  last_update_time <- file.info(local_path)$mtime
  
  since - last_update_time
}

# Standardize varieties of network inputs
translate_network <- function(network) {
  allowed_networks <- list(
    agency = c("fem", "naps", "agency", "fems"),
    purpleair = c("pa", "purpleair", "pas", "purpleairs"),
    aqegg = c("aqegg", "egg", "eggs")
  )
  network <- allowed_networks |>
    sapply(\(x) tolower(network) %in% x)
  if (!any(network)) {
    stop("Network not supported")
  }
  names(network)[which(network)]
}
