#' Get NOAA HMS smoke polygons
#'
#' The National Oceanic and Atmospheric Administration provides smoke polygons based on visual analysis of satellite data for the Hazard Mapping System (HMS)
#'
#' From the HMS website:
#'
#' HMS's smoke analysis is based on visual classification of plumes using GOES-East and GOES-West ABI true-color imagery
#' available during the sunlit part of the orbit.
#' Since the analysis generally requires a sequential set of satellite images to help distinguish smoke from clouds and other atmospheric aerosols,
#' the first smoke analysis for the current day is usually produced around the local noon time – until then,
#' only fire detection points may be available. Additional smoke analysis will occur throughout the day until sunset or as observation conditions permit.
#'
#' See \href{https://www.ospo.noaa.gov/products/land/hms.html#about}{here} for more information.
#'
#' @inheritParams get_eccc_eer_smoke_forecasts
#' @export
#'
#' @examples
#' hms <- get_noaa_hms_smoke_polygons()
#' make_leaflet_map(
#'   polygon_data = list("HMS Smoke" = hms),
#'   polygon_options = list(
#'     fillColor = ~ density,
#'     weight = 1,
#'     color = "black",
#'     fillOpacity = 0.8,
#'     opacity = 1,
#'     label = ~ paste("Satellite(s): ", satellite, "<br/>", "Period: ", period, "<br/>", "Density: ", density) |>
#'       lapply(htmltools::HTML),
#'     palette = hms_smoke_pal()
#'   )
#' )
get_noaa_hms_smoke_polygons <- function(
  select_times = Sys.time(),
  data_dir = tempdir(),
  quiet = FALSE
) {
  desired_cols <- c(
    satellite = "Satellite",
    "period",
    "density" = "Density"
  )

  # Download and unzip new runs as needed
  select_times |>
    get_hms_zip(
      data_dir = data_dir,
      quiet = quiet
    )

  # Build shp file name path
  shape_names <- "hms_smoke%s.shp" |>
    sprintf(format(select_times, "%Y%m%d"))
  shape_paths <- data_dir |>
    file.path(shape_names)

  # Load shapefile(s) and cleanup
  hms_smoke <- shape_paths |>
    handyr::for_each(
      sf::read_sf,
      .bind = TRUE,
      .show_progress = FALSE
    ) |>
    dplyr::mutate(
      # Format dates properly
      dplyr::across(
        c(Start, End),
        ~ lubridate::as_datetime(.x, format = "%Y%j %H%M", tz = "UTC")
      ),
      period = lubridate::interval(Start, End),
      # Set density to factor
      Density = factor(Density, levels = c("Heavy", "Medium", "Light")),
    ) |>
    dplyr::filter(!sf::st_is_empty(geometry)) |>
    sf::st_make_valid() |>
    # Combine into multipolygons by period/density
    dplyr::summarise(
      .by = c(period, Density),
      Satellite = unique(Satellite) |> paste(collapse = " + "),
      geometry = sf::st_union(geometry)
    ) |>
    # Select/rename columns
    dplyr::select(dplyr::all_of(desired_cols))

  # Remove overlap of polygons so opacity works properly
  hms_smoke |>
    split(~period) |>
    handyr::for_each(
      \(fcst_data) {
        fcst_data |>
          dplyr::arrange(dplyr::desc(density)) |>
          sf::st_transform(3857) |>
          sf::st_difference() |>
          sf::st_transform("WGS84") |>
          dplyr::arrange(density)
      },
      .bind = TRUE,
      .show_progress = FALSE
    )
}

#' Colour palette for HMS smoke polygons
#'
#' Matches the colour scheme provided \href{https://www.ospo.noaa.gov/products/land/hms.html#maps}{here}.
#' @param hms_smoke_density (Optional).
#'   Either NULL (the default), which returns the leaflet palette function,
#'   or a character/factor vector of smoke densities from HMS smoke polygons.
#'   Allowed densities (English/French) are "Light"/"Faible", "Medium"/"Moyen", and "Heavy"/"Haute".
#' @return A leaflet palette function or a character vector of hex colours corresponding to values in `hms_smoke_density`
#' @source \href{https://www.ospo.noaa.gov/products/land/hms.html#maps}{HMS smoke polygons}
#' @export
hms_smoke_pal <- function(hms_smoke_density = NULL) {
  colours <- c(
    "Light" = "#8CF183",
    "Faible" = "#8CF183",
    "Medium" = "#D7FC6B",
    "Moyen" = "#D7FC6B",
    "Heavy" = "#E19651",
    "Haute" = "#E19651"
  )
  pal <- leaflet::colorFactor(
    palette = unname(colours),
    levels = names(colours),
    ordered = TRUE
  )
  if (is.null(hms_smoke_density)) {
    return(pal)
  }
  pal(as.character(hms_smoke_density))
}

get_hms_zip <- function(
  select_times = Sys.time(),
  data_dir = tempdir(),
  unzip = TRUE,
  quiet = FALSE
) {
  # Build url to desired zip file(s)
  select_times <- select_times |> lubridate::with_tz("America/New_York")
  is_todays <- select_times >= lubridate::with_tz(Sys.Date(), "UTC")
  source_template <- "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/%s/%s/%s.zip"
  shape_names <- paste0("hms_smoke", format(select_times, "%Y%m%d"))

  zip_urls <- source_template |>
    sprintf(
      lubridate::year(select_times),
      format(select_times, "%m"),
      shape_names
    )

  # Download and unzip new runs as needed
  run_zips <- data_dir |>
    file.path(
      paste0("hms_", format(select_times, "%Y%m%d"), "_shp.zip")
    )
  zip_urls |>
    handyr::for_each(.enumerate = TRUE, \(zip_url, i) {
      if (is_todays[i] || !file.exists(run_zips[i])) {
        zip_url |>
          download.file(destfile = run_zips[i], mode = "wb", quiet = quiet)
        if (unzip) unzip(run_zips[i], exdir = data_dir)
      }
    })
}
