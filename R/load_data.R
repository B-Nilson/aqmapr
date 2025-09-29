#' Load most recent AQmap observation data
#'
#' @description
#' Load the most recent AQmap observation data from
#' https://aqmap.ca/aqmap/aqmap_most_recent_obs.Rds 
#' which is updated every 10 minutes alongside AQmap.
#' 
#' Alternativley, you can run [start_server] to host an AQmapr app locally,
#' which exposes the results of this function (by default) at 127.0.0.1:8000/data/recent/:type
#' where :type is one of "json", "csv", or "tsv".
#'
#' @param allowed_networks (Optional).
#'   A list of allowed network names to assing each monitor type ("fem", "pa", "egg") to.
#'   Default groups "fem" monitors as "agency", and "pa" + "egg" monitors as "lcm".
#' @param desired_cols (Optional).
#'   A vector of column names to load.
#'   Value names, where present, will be used to rename that column.
#'   Default loads all columns for use in [aqmapr::make_aqmap].
#' @param id_columns (Optional).
#'   A vector of column names to use as unique identifiers.
#'   Will drop any rows with missing values for these columns.
#'   Default is c("site_id", "network", "lat", "lng", "date_last_obs").
#' @param data_dir (Optional).
#'   The directory to write the downloaded data file to (acts as a cache).
#'   Default is a temporary directory [see [tempdir]].
#'
#' @export
load_recent_aqmap_data <- function(
  network_monitors = list(
    agency = "fem",
    lcm = c("pa", "egg")
  ),
  desired_cols = c(
    site_id = "sensor_index",
    name = "monitor",
    "network",
    "monitor_type",
    "lat",
    "lng",
    "prov_terr",
    date_last_obs = "date",
    pm25_10min = "pm25_recent",
    "pm25_1hr",
    "pm25_3hr",
    "pm25_24hr"
  ),
  id_columns = c("site_id", "network", "lat", "lng", "date_last_obs"),
  data_dir = tempdir()
) {
  stopifnot(length(data_dir) == 1, is.character(data_dir))
  # remaining args are validated in format_recent_aqmap_data

  # Data source information
  aqmap_url <- "https://aqmap.ca/aqmap"
  file_name <- "aqmap_most_recent_obs.Rds"

  # Create data directory if needed
  if (!dir.exists(data_dir)) {
    if (interactive()) {
      # Check user okay (if user present)
      response <- readline(
        prompt = paste0("Create directory", data_dir, "? [y/n] ")
      )
      if (tolower(response) != "y") {
        stop("User declined to create directory")
      }
    }
    dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
  }

  # Determine time since local file was updated,
  # (relative to when file should have been last updated)
  local_path <- file.path(data_dir, file_name)
  local_file_age <- local_path |>
    get_file_age(
      since = lubridate::now(tz = "UTC") |>
        lubridate::floor_date("10 mins")
    )

  # Download .rds file if needed
  if (local_file_age > "10 mins") {
    aqmap_url |>
      file.path("data", file_name) |>
      utils::download.file(destfile = local_path, mode = "wb")
  }

  # Load and cleanup
  readRDS(local_path) |>
    format_recent_aqmap_data(
      desired_cols = desired_cols,
      id_columns = id_columns,
      network_monitors = network_monitors
    )
}

format_recent_aqmap_data <- function(
  aqmap_data,
  desired_cols,
  id_columns,
  network_monitors
) {
  stopifnot(
    length(network_monitors) > 0,
    is.list(network_monitors),
    is.character(unlist(network_monitors))
  )
  stopifnot(length(desired_cols) > 0, is.character(desired_cols))
  stopifnot(
    length(id_columns) > 0,
    is.character(id_columns),
    all(id_columns %in% c(desired_cols, names(desired_cols)))
  )

  aqmap_data |>
    dplyr::mutate(
      monitor_type = .data$network |>
        factor(c("FEM", "PA", "EGG")),
      network = .data$network |>
        translate_network(
          allowed_networks = network_monitors,
          as_factor = TRUE
        ),
      pm25_recent = ifelse(
        .data$network == "agency",
        NA_real_,
        .data$pm25_recent
      ),
    ) |>
    dplyr::select(dplyr::any_of(desired_cols)) |>
    dplyr::filter(stats::complete.cases(
      dplyr::across(dplyr::all_of(id_columns))
    ))
}

# Read in AQmap plot data (hourly data for past 30+ days)
# for a given network (ex. fem) and site id (ex. 10102)
load_aqmap_plot_data <- function(
  network,
  site_id,
  allowed_networks
) {
  stopifnot(length(network) == 1, is.character(network))
  stopifnot(length(site_id) == 1, is.character(site_id) | is.numeric(site_id))

  aqmap_url <- "https://aqmap.ca/aqmap"
  plot_data_url <- file.path(aqmap_url, "data/plotting")
  plot_file_template <- "%s_recent_hourly.csv"

  # Handle aliases for network
  network <- network |>
    translate_network(
      allowed_networks = allowed_networks,
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
