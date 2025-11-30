#' Get ECCC EER smoke forecasts
#'
#' The Environmental Emergency Response group at Environment and Climate Change Canada (ECCC) provides smoke forecasts for Canada.
#' They run dispersion models using fire information and ECCC weather models.
#' See \href{https://eer.cmc.ec.gc.ca/mandats/AutoSim/Fire/}{here} for more information.
#'
#' @param select_times POSIXct time(s) to download data for
#' @param region String specifying the region to download data for. Currently only "Canada" is supported.
#' @param data_dir Directory to store downloaded data in. Defaults to tempdir().
#' @param quiet Logical. Should output from `download.file` be suppressed?
#' @export
#'
#' @examples
#' eer <- get_eccc_eer_smoke_forecasts()
#' make_leaflet_map(
#'   polygon_data = list("EER Smoke" = eer),
#'   polygon_options = list(
#'     fillColor = ~ min_pm25,
#'     weight = 1,
#'     color = "black",
#'     fillOpacity = 0.8,
#'     opacity = 1,
#'     palette = eer_smoke_pal()
#'   )
#' )
get_eccc_eer_smoke_forecasts <- function(
  select_times = Sys.time(),
  region = "Canada",
  data_dir = tempdir(),
  quiet = FALSE
) {
  # cat(paste0('c("', paste(get_eer_regions(Sys.time()), collapse = '", "'), '")'))
  stopifnot(lubridate::is.POSIXct(select_times), length(select_times) > 0)
  stopifnot(
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
  stopifnot(is.character(data_dir), length(data_dir) == 1)

  desired_cols <- c(
    "region",
    "model_time",
    "forecast_time",
    min_pm25 = "Interval",
    altitude = "Height"
  )

  # Floor to nearest UTC hour
  select_times <- select_times |>
    lubridate::with_tz("UTC") |>
    lubridate::floor_date("hours") |>
    unique()

  # Download and unzip new runs as needed
  select_times |>
    lubridate::floor_date("6 hours") |>
    get_eer_zip(data_dir = data_dir, region = region, quiet = quiet)

  # Build shp file name path
  shape_names <- "shp_%s_%s" |>
    sprintf(region, format(select_times, "%Y%m%d-%H%M"))
  shape_paths <- data_dir |>
    file.path(shape_names, paste0(shape_names, ".shp"))

  # Load shapefile, convert to POLYGON and cleanup
  eer_smoke <- shape_paths |>
    handyr::for_each(
      \(path, i) {
        path |>
          sf::read_sf() |>
          # Add useful info
          dplyr::mutate(
            Height = Height |> units::set_units("m"),
            model_time = select_times[i] |>
              lubridate::floor_date("6 hours"),
            forecast_time = select_times[i],
            region = region
          )
      },
      .bind = TRUE,
      .show_progress = FALSE,
      .enumerate = TRUE
    ) |>
    dplyr::filter(!sf::st_is_empty(geometry)) |>
    # LINESTRING -> POLYGON
    sf::st_cast("POLYGON") |>
    sf::st_make_valid() |>
    # Select/rename columns
    dplyr::select(dplyr::all_of(desired_cols))

  # Remove overlap of polygons so opacity works properly
  eer_smoke |>
    split(~forecast_time) |>
    handyr::for_each(
      \(fcst_data) {
        fcst_data |>
          dplyr::arrange(dplyr::desc(min_pm25)) |>
          sf::st_transform(3857) |>
          sf::st_difference() |>
          sf::st_transform("WGS84") |>
          dplyr::arrange(min_pm25)
      },
      .bind = TRUE,
      .show_progress = FALSE
    )
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

get_eer_zip <- function(
  model_runs,
  region = "Canada",
  data_dir = tempdir(),
  unzip = TRUE,
  quiet = FALSE
) {
  is_todays <- model_runs >= lubridate::with_tz(Sys.Date(), "UTC")
  if (any(is_todays)) {
    is_todays <- c(is_todays, TRUE)
  }

  # Build url to this runs zip file
  zip_urls <- model_runs |>
    make_eer_zip_dir() |>
    paste0("shp_", region, ".zip")

  # Download and unzip new runs as needed
  unzip_details <- zip_urls |>
    stringr::str_extract(
      paste0(region, "/(.+?)/shp/(shp_.+?)\\.zip"),
      group = 1:2
    )
  local_paths <- data_dir |>
    file.path(unzip_details[, 1] |> paste0("_", unzip_details[, 2], ".zip"))
  if (!quiet) {
    rlang::check_installed("pbapply")
  }
  zip_urls |>
    handyr::for_each(
      .enumerate = TRUE,
      .show_progress = !quiet,
      \(zip_url, i) {
        if (is_todays[i] || !file.exists(local_paths[i])) {
          success <- zip_url |>
            download.file(
              destfile = local_paths[i],
              mode = "wb",
              quiet = quiet
            ) |>
            suppressWarnings() |>
            handyr::on_error(.return = NULL) # Fails near 24 UTC when latest transitions to next day
          if (unzip & !is.null(success)) unzip(local_paths[i], exdir = data_dir)
        }
      }
    )
}

make_eer_zip_dir <- function(model_runs) {
  is_todays <- model_runs >= lubridate::with_tz(Sys.Date(), "UTC")
  source_template <- "https://eer.cmc.ec.gc.ca/mandats/AutoSim/Fire/%s/Canada/%s/shp/"
  timestamps <- format(model_runs, "%Y%m%d.%H00")
  if (any(is_todays)) {
    timestamps <- c(timestamps, "latest")
  }
  source_template |>
    sprintf(format(model_runs, "%HUTC"), timestamps) |>
    unique()
}

get_eer_regions <- function(select_time) {
  zip_dir <- select_time |>
    lubridate::floor_date("6 hours") |>
    make_eer_zip_dir()
  zip_dir |>
    url() |>
    readLines() |>
    stringr::str_subset("href=\".*zip\"") |>
    stringr::str_extract("href=\"shp_(.*?).zip\"", group = 1) |>
    unique()
}
