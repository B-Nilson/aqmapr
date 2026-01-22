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
#'   polygon_layers = list(PolygonLayer(
#'     group = "HMS Smoke",
#'     data = hms,
#'     label = ~ paste(
#'       "Satellite(s): ", satellite, "<br/>",
#'       "Period: ", period, "<br/>",
#'       "Density: ", density
#'     ),
#'     fill = ~ density,
#'     fill_palette = hms_smoke_pal()
#'   ))
#' )
get_noaa_hms_smoke_polygons <- function(
  select_time = Sys.time(),
  data_dir = tempdir(),
  quiet = FALSE
) {
  desired_cols <- c(
    satellite = "Satellite",
    "period",
    density = "Density"
  )

  # Download and unzip as needed
  hms_files <- select_time |>
    get_hms_zip(data_dir = data_dir, quiet = quiet)
  shape_path <- hms_files[endsWith(hms_files, ".shp")]

  # Load shapefile and cleanup
  hms_smoke <- shape_path |> read_hms_shp()

  # Handle no rows/columns (replace with NULL)
  is_empty <- nrow(hms_smoke) == 0 | ncol(hms_smoke) == 0
  if (is_empty) {
    hms_smoke <- NULL
  }

  # Combine start/end date and select/rename columns
  hms_smoke |>
    dplyr::mutate(
      period = .data[[date_cols[1]]] |>
        lubridate::interval(.data[[date_cols[2]]])
    ) |>
    dplyr::select(dplyr::all_of(desired_cols))
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
  if (!quiet) {
    rlang::check_installed("pbapply")
  }
  zip_urls |>
    handyr::for_each(
      .enumerate = TRUE,
      .show_progress = !quiet,
      .as_list = TRUE,
      \(zip_url, i) {
        if (is_todays[i] || !file.exists(run_zips[i])) {
          zip_url |>
            utils::download.file(
              destfile = run_zips[i],
              mode = "wb",
              quiet = quiet
            )
          if (unzip) unzip(run_zips[i], exdir = data_dir)
        }
      }
    ) |>
    unlist()
}

read_hms_shp <- function(
  shp_path,
  date_fmt = "%Y%j %H%M",
  date_tz = "UTC",
  date_cols = c("Start", "End"),
  density_levels = c("Heavy", "Medium", "Light")
) {
  shp_path |>
    sf::read_sf() |>
    dplyr::mutate(
      # Format dates properly
      dplyr::across(
        dplyr::all_of(date_cols),
        \(x) x |> lubridate::as_datetime(format = date_fmt, tz = date_tz)
      ),
      # Set density to factor
      Density = .data$Density |> factor(levels = density_levels),
    ) |>
    # Remove empty geometries and fix invalid geometries
    dplyr::filter(!sf::st_is_empty(.data$geometry)) |>
    sf::st_make_valid() |>
    # Combine into multipolygons by period/density
    dplyr::summarise(
      .by = dplyr::all_of(c("Density", date_cols)),
      Satellite = .data$Satellite |> unique() |> paste(collapse = " + "),
      geometry = .data$geometry |> sf::st_union()
    ) |>
    # Sort by period/density
    dplyr::arrange(
      dplyr::pick(dplyr::all_of(date_cols)),
      dplyr::desc(.data$Density)
    ) |>
    # Remove overlap of polygons so opacity works properly
    dplyr::group_split(dplyr::pick(dplyr::all_of(date_cols))) |>
    lapply(
      \(fcst_data) {
        fcst_data |>
          sf::st_transform(3857) |>
          sf::st_difference() |>
          sf::st_transform("WGS84") |>
          dplyr::arrange(.data$Density)
      }
    ) |>
    dplyr::bind_rows()
}
