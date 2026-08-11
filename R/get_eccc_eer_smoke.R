#' Get ECCC EER smoke forecast
#'
#' The Environmental Emergency Response group at Environment and Climate Change Canada (ECCC) provides smoke forecasts for Canada.
#' They run dispersion models using fire information and ECCC weather models.
#' See \href{https://eer.cmc.ec.gc.ca/mandats/AutoSim/Fire/}{here} for more information.
#'
#' @details
#' Only the most recently posted run is available for the current day, and
#' archived runs are kept for only the most recent ~8 days. EER shapefiles
#' omit the initial hour of each model run (the model forecasts from the next
#' hour onwards), so `select_time`s that fall exactly on a run start
#' (00/06/12/18 UTC) are served from the previous run, which forecast that
#' hour as its +6h output.
#'
#' @param select_time POSIXct time to download data for
#' @param region String specifying the EER region to download data for (e.g. "Canada").
#'   See `check_eer_region()` for the full list of supported regions.
#' @param data_dir Directory to store downloaded data in. Defaults to tempdir().
#' @param quiet Logical. Should output from `download.file` be suppressed?
#' @param cache Logical. Should the downloaded data be cached? Defaults to TRUE.
#' @param archive_days Number of days of past model runs to allow.
#'   ECC only archives the most recent ~8 days of runs, so requesting an older
#'   `select_time` errors with a clear message. Defaults to 8.
#' @param timeout Number of seconds to allow for downloading the shapefile zip.
#'   Defaults to `max(getOption("timeout"), 300)`. Increase this if downloads
#'   fail on a slow connection.
#' @param cache_refresh_hours How old (in hours) the cached copy of the current
#'   day's forecast can be before it is re-downloaded. The current day's data is
#'   served from ECC's `latest` alias, which switches to a newer run once it is
#'   posted, so an old cached copy quickly goes stale. Archived runs never change
#'   and stay cached indefinitely. Defaults to 1. Use `Inf` to never refresh the
#'   current day's file, or `0` to always re-download it.
#' @param cleanup_keep_hours How old (in hours) cached EER files must be before
#'   they are removed. Defaults to 24. Use `Inf` to keep all files.
#' @export
#'
#' @examples
#' eer <- get_eccc_eer_smoke()
#' make_leaflet_map(
#'   polygon_layers = list(PolygonLayer(
#'     group = "EER Smoke",
#'     data = eer,
#'     fill = ~ min_pm25,
#'     fill_palette = eer_smoke_pal(),
#'     display_by_default = FALSE
#'   ))
#' )
get_eccc_eer_smoke <- function(
  select_time = Sys.time(),
  region = "Canada",
  data_dir = tempdir(),
  quiet = FALSE,
  cache = TRUE,
  archive_days = 8,
  timeout = max(getOption("timeout"), 300),
  cache_refresh_hours = 1,
  cleanup_keep_hours = 24
) {
  stopifnot(lubridate::is.POSIXct(select_time), length(select_time) == 1)
  check_eer_region(region)
  stopifnot(is.character(data_dir), length(data_dir) == 1)
  stopifnot(is.logical(quiet), length(quiet) == 1)
  stopifnot(
    is.numeric(archive_days),
    length(archive_days) == 1,
    archive_days > 0
  )
  stopifnot(is.numeric(timeout), length(timeout) == 1, timeout > 0)
  stopifnot(
    is.numeric(cache_refresh_hours),
    length(cache_refresh_hours) == 1,
    cache_refresh_hours >= 0
  )
  stopifnot(
    is.numeric(cleanup_keep_hours),
    length(cleanup_keep_hours) == 1,
    cleanup_keep_hours >= 0
  )

  desired_cols <- c(
    "region",
    "model_time",
    "forecast_time",
    min_pm25 = "Interval",
    altitude = "Height"
  )

  # Floor to nearest UTC hour
  select_time <- select_time |>
    lubridate::with_tz("UTC") |>
    lubridate::floor_date("hours")
  model_run <- eer_model_run(select_time)
  # The current day's forecast comes from the mutable `latest` alias, while
  # archived runs never change
  is_todays <- model_run >= (lubridate::today(tzone = "UTC") |> lubridate::as_datetime())

  # Build url to this runs zip file
  zip_url <- "%s/shp_%s.zip" |>
    sprintf(make_eer_zip_dir(model_run, archive_days = archive_days), region)
  local_path <- "%s/eer_%s_%s_shp.zip" |>
    sprintf(data_dir, region, format(model_run, "%Y%m%d-%H%M"))
  shp_pattern <- ".*%s\\.shp$" |> sprintf(format(select_time, "%Y%m%d-%H00"))
  # Per-run extraction directory, so cached calls can skip re-extracting the
  # ~48 per-hour folders that `get_and_unzip` unpacks each time
  extract_dir <- "%s/eer_%s_%s_shp" |>
    sprintf(data_dir, region, format(model_run, "%Y%m%d-%H%M"))

  # Refresh the current day's cached `latest` zip once it goes stale. When
  # refreshing, also drop the extracted folders: a new run reuses the same
  # per-hour folder names with different content, so they must be re-extracted.
  if (cache_file_stale(local_path, is_todays, cache, cache_refresh_hours)) {
    unlink(local_path)
    unlink(extract_dir, recursive = TRUE)
  }

  # Remove old cached EER files (zips and extracted per-run folders) on exit
  if (!is.infinite(cleanup_keep_hours)) {
    on.exit(try(clean_eer_files(data_dir, cleanup_keep_hours), silent = TRUE), add = TRUE)
  }

  # Download and unzip the run's shapefiles
  old_timeout <- getOption("timeout")
  options(timeout = timeout)
  on.exit(options(timeout = old_timeout), add = TRUE)

  # If this run's shapefiles are already extracted, reuse them without
  # downloading or unzipping again
  shp_paths <- list.files(
    extract_dir,
    pattern = shp_pattern,
    recursive = TRUE,
    full.names = TRUE
  )
  if (!cache || length(shp_paths) == 0) {
    shp_paths <- tryCatch(
      get_and_unzip_retry(
        zip_url = zip_url,
        local_path = local_path,
        unzip_dir = extract_dir,
        cache = cache,
        quiet = quiet,
        pattern = shp_pattern
      ),
      error = function(e) {
        # Drop any partial download so a cached retry re-downloads cleanly
        unlink(local_path)
        stop(
          sprintf(
            "Failed to get EER smoke forecast for %s: %s. Note that ECC only archives the most recent ~%d days of runs.",
            format(model_run, "%Y-%m-%d %H:%M UTC"),
            conditionMessage(e),
            archive_days
          ),
          call. = FALSE
        )
      }
    )
  }

  # No shapefile for the requested hour, so treat it like an empty forecast
  if (length(shp_paths) == 0) {
    warning("No layers in this run's EER smoke forecast, returning NULL.")
    return(NULL)
  }

  # Read and clean the shapefile
  eer_smoke <- tryCatch(
    read_eer_shp(shp_paths, model_run = model_run),
    error = function(e) {
      # A corrupt extracted shapefile should not poison the cache: drop it (and
      # the zip) so the next call re-downloads instead of failing on it forever
      unlink(local_path)
      unlink(extract_dir, recursive = TRUE)
      stop(
        sprintf(
          "Failed to read EER smoke forecast for %s: %s.",
          format(model_run, "%Y-%m-%d %H:%M UTC"),
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )

  # Handle no rows/columns (replace with NULL)
  is_empty <- nrow(eer_smoke) == 0 | ncol(eer_smoke) == 0 | is.null(eer_smoke)
  if (is_empty) {
    warning("No layers in this run's EER smoke forecast, returning NULL.")
    return(NULL)
  }

  # Add metadata and sort/rename cols
  eer_smoke |>
    dplyr::mutate(
      model_time = model_run,
      forecast_time = select_time,
      region = region
    ) |>
    dplyr::select(dplyr::all_of(desired_cols))
}

# Pick the model run whose forecast covers `select_time`. EER zips omit the
# initial hour of each run (the model forecasts from the next hour onwards),
# so a `select_time` that falls exactly on a run start (00/06/12/18 UTC) has
# no forecast in that run; fall back to the previous run, which forecast that
# hour as its +6h output.
eer_model_run <- function(select_time) {
  select_time <- select_time |>
    lubridate::with_tz("UTC") |>
    lubridate::floor_date("hours")
  model_run <- select_time |>
    lubridate::floor_date("6 hours")
  if (model_run == select_time) {
    model_run <- model_run - lubridate::hours(6)
  }
  model_run
}

#' Colour palette for EER smoke forecasts
#'
#' Matches the colour scheme provided \href{https://eer.cmc.ec.gc.ca/mandats/AutoSim/Fire/latest/Canada/latest/img/Canada/anim.html}{here}.
#' @param eer_pm25_ugm3 (Optional).
#'   Either NULL (the default), which returns the leaflet palette function,
#'   or a numeric vector of PM2.5 concentrations from EER smoke forecasts.
#' @return A leaflet palette function or a character vector of hex colours corresponding to values in `eer_pm25_ugm3`
#' @source \href{https://eer.cmc.ec.gc.ca/mandats/AutoSim/Fire/latest/Canada/latest/img/Canada/anim.html}{EER smoke forecasts}
#' @export
eer_smoke_pal <- function(eer_pm25_ugm3 = NULL) {
  colours <- c(
    "#DEDEDE" = 5,
    "#BBBBBB" = 10,
    "#B1E7FF" = 25,
    "#5AB0FF" = 35,
    "#BDFF7B" = 50,
    "#5ADE5A" = 75,
    "#FFFF5A" = 100,
    "#FFAC5A" = 200,
    "#C48F5A" = 300,
    "#FFA7FF" = 500
  )
  pal <- leaflet::colorBin(
    bins = unname(colours) |> c(Inf),
    palette = names(colours)
  )
  if (is.null(eer_pm25_ugm3)) {
    return(pal)
  }
  pal(eer_pm25_ugm3)
}

read_eer_shp <- function(shp_path, model_run) {
  if (is.null(shp_path) || length(shp_path) == 0) {
    stop("No EER shapefile paths to read.", call. = FALSE)
  }
  # read in, drop empty, convert to POLYGON from LINESTRING
  eer_raw <- shp_path |>
    sf::read_sf() |>
    dplyr::mutate(Height = .data$Height |> units::set_units("m")) |>
    dplyr::filter(!sf::st_is_empty(.data$geometry)) |>
    sf::st_cast("POLYGON") |>
    sf::st_make_valid()
  if (nrow(eer_raw) == 0) {
    return(eer_raw)
  }

  # Remove overlap of polygons so opacity works properly
  eer_raw |>
    dplyr::arrange(dplyr::desc(.data$Interval)) |>
    dplyr::group_by(.data$Height) |>
    remove_polygon_overlap()
}

# Remove cached EER zips and extracted per-run folders older than `keep_hours`
clean_eer_files <- function(data_dir, keep_hours) {
  artifacts <- list.files(
    data_dir,
    pattern = "^(eer_.*_shp\\.zip|shp_.*_[0-9]{8}-[0-9]{4}|eer_.*_[0-9]{8}-[0-9]{4}_shp)$",
    full.names = TRUE
  )
  ages_hours <- artifacts |>
    purrr::map_dbl(\(f) get_file_age(f) |> as.numeric(units = "hours"))
  old <- ages_hours > keep_hours
  if (any(old)) {
    unlink(artifacts[old], recursive = TRUE)
  }
  invisible(artifacts[old])
}

make_eer_zip_dir <- function(model_run, archive_days = 8) {
  source_template <- "https://eer.cmc.ec.gc.ca/mandats/AutoSim/Fire/%s/Canada/%s/shp"
  today_utc <- lubridate::today(tzone = "UTC") |>
    lubridate::as_datetime()
  if (model_run >= today_utc) {
    # Use the top-level "latest" alias: the most recently posted run
    source_template |>
      sprintf("latest", "latest")
  } else {
    # Archived runs are kept for only a rolling ~`archive_days` day window
    archive_cutoff <- today_utc - lubridate::days(archive_days)
    if (model_run < archive_cutoff) {
      stop(
        sprintf(
          "EER smoke forecasts are only archived for the most recent %d days (requested run: %s). Select a more recent time.",
          archive_days,
          format(model_run, "%Y-%m-%d %H:%M UTC")
        ),
        call. = FALSE
      )
    }
    source_template |>
      sprintf(
        format(model_run, "%HUTC"),
        format(model_run, "%Y%m%d.%H00")
      )
  }
}

check_eer_region <- function(region) {
  stopifnot(
    length(region) == 1,
    region %in%
      c(
        "Canada",
        "AB_SK_North",
        "AB_SK_South",
        "Atlantic_East",
        "Atlantic_Labrador",
        "Atlantic_West",
        "BC_AB_North",
        "BC_North",
        "BC_South",
        "BC_South_Cranbrook",
        "BC_South_Lytton",
        "BC_South_Williams_Lake",
        "Great_Lakes",
        "MB_ON_South",
        "NB_CFB_Gagetown",
        "NS_South",
        "NT",
        "NU",
        "ON_North",
        "QC_Central",
        "QC_North",
        "QC_South",
        "SK_MB_North",
        "SK_MB_South",
        "YT",
        "YT_North"
      )
  )
}
