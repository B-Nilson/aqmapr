#' Get ECCC EER smoke forecast
#'
#' The Environmental Emergency Response group at Environment and Climate Change Canada (ECCC) provides smoke forecasts for Canada.
#' They run dispersion models using fire information and ECCC weather models.
#' See \href{https://eer.cmc.ec.gc.ca/mandats/AutoSim/Fire/}{here} for more information.
#'
#' @param select_time POSIXct time to download data for
#' @param region String specifying the region to download data for. Currently only "Canada" is supported.
#' @param data_dir Directory to store downloaded data in. Defaults to tempdir().
#' @param quiet Logical. Should output from `download.file` be suppressed?
#' @param cache Logical. Should the downloaded data be cached? Defaults to TRUE.
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
  cache = TRUE
) {
  stopifnot(lubridate::is.POSIXct(select_time), length(select_time) == 1)
  check_eer_region(region)
  stopifnot(is.character(data_dir), length(data_dir) == 1)
  stopifnot(is.logical(quiet), length(quiet) == 1)

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
  model_run <- select_time |>
    lubridate::floor_date("6 hours")

  # Build url to this runs zip file
  zip_url <- "%s/shp_%s.zip" |>
    sprintf(make_eer_zip_dir(model_run), region)
  local_path <- "%s/eer_%s_%s_shp.zip" |>
    sprintf(data_dir, region, format(model_run, "%Y%m%d-%H%M"))
  shp_pattern <- ".*%s\\.shp$" |> sprintf(format(select_time, "%Y%m%d-%H00"))

  # Download, unzip, read
  eer_smoke <- zip_url |>
    handyr::get_and_unzip(
      local_path = local_path,
      unzip_dir = data_dir,
      cache = cache,
      quiet = quiet
    ) |>
    stringr::str_subset(pattern = shp_pattern) |>
    read_eer_shp(model_run = model_run)

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

make_eer_zip_dir <- function(model_run) {
  source_template <- "https://eer.cmc.ec.gc.ca/mandats/AutoSim/Fire/%s/Canada/%s/shp/"
  is_todays <- model_run >= lubridate::today(tzone = "UTC")
  if (any(is_todays)) {
    timestamp <- "latest"
  } else {
    timestamp <- format(model_run, "%Y%m%d.%H00")
  }
  source_template |>
    sprintf(format(model_run, "%HUTC"), timestamp)
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
