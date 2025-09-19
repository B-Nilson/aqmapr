# Download and read in most recent AQmap obs datafile
load_recent_aqmap_data <- function(data_dir, aqmap_url, desired_cols) {
  stopifnot(length(data_dir) == 1, is.character(data_dir))
  stopifnot(length(aqmap_url) == 1, is.character(aqmap_url))
  stopifnot(length(desired_cols) > 0, is.character(desired_cols))

  file_name <- "aqmap_most_recent_obs.Rds"
  local_path <- file.path(data_dir, file_name)

  # Determine time since local file was updated,
  # (relative to when file should have been last updated)
  newest_file <- lubridate::now(tz = "UTC") |>
    lubridate::floor_date("10 mins")
  local_file_age <- get_file_age(local_path, since = newest_file)

  # Download .rds file if needed
  if (local_file_age > "10 mins") {
    aqmap_url |>
      file.path("data", file_name) |>
      utils::download.file(local_path, mode = "wb")
  }

  # Load and cleanup
  obs <- readRDS(local_path) |>
    dplyr::mutate(monitor_type = .data$network) |>
    dplyr::select(dplyr::any_of(desired_cols)) |>
    # TODO: refactor
    dplyr::filter(stats::complete.cases(
      .data$site_id,
      .data$network,
      .data$lat,
      .data$lng,
      .data$date_last_obs
    )) |>
    dplyr::mutate(
      network = .data$network |>
        translate_network(
          allowed_networks = .cst$allowed_networks,
          as_factor = TRUE
        ),
      pm25_10min = ifelse(
        .data$network == "agency",
        NA_real_,
        .data$pm25_10min
      ),
    )
  obs$pm25_1hr[is.nan(obs$pm25_1hr)] <- NA_real_
  return(obs)
}

# Read in AQmap plot data (hourly data for past 30+ days)
# for a given network (ex. fem) and site id (ex. 10102)
load_aqmap_plot_data <- function(
  network,
  site_id,
  aqmap_url
) {
  stopifnot(length(network) == 1, is.character(network))
  stopifnot(length(site_id) == 1, is.character(site_id) | is.numeric(site_id))
  stopifnot(length(aqmap_url) == 1, is.character(aqmap_url))

  plot_data_url <- file.path(aqmap_url, "data/plotting")
  plot_file_template <- "%s_recent_hourly.csv"

  # Handle aliases for network
  network <- network |>
    translate_network(
      allowed_networks = .cst$allowed_networks,
      group_lcms = FALSE
    )

  # Build desired file url
  if (network != "agency") {
    # Prepend "sensor_" for non-agency networks
    site_id <- paste0("sensor_", site_id)
  }
  file_name <- plot_file_template |>
    sprintf(site_id)

  # Read in data
  plot_data_url |>
    file.path(network, file_name) |>
    data.table::fread()
}
