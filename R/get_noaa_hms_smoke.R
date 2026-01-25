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
#' @inheritParams get_eccc_eer_smoke
#' @export
#'
#' @examples
#' hms <- get_noaa_hms_smoke()
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
get_noaa_hms_smoke <- function(
  select_time = Sys.time(),
  data_dir = tempdir(),
  quiet = FALSE
) {
  desired_cols <- c(
    satellite = "Satellite",
    "period",
    density = "Density"
  )

  select_time <- select_time |> lubridate::with_tz("America/Vancouver")
  shape_date <- select_time |> format("%Y%m%d")
  shape_month <- select_time |> format("%Y/%m")
  is_todays <- select_time >= lubridate::today(tzone = "UTC")

  # Build url to desired zip file
  source_url <- "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile"
  zip_url <- "%s/%s/hms_smoke%s.zip" |>
    sprintf(source_url, shape_month, shape_date)

  # Download, unzip, read
  local_path <- "%s/hms_%s_shp.zip" |>
    sprintf(data_dir, shape_date)
  hms_smoke <- zip_url |>
    handyr::get_and_unzip(
      local_path = local_path,
      unzip_dir = data_dir,
      cache = cache,
      quiet = quiet
    ) |>
    stringr::str_subset(pattern = ".*\\.shp$") |>
    read_hms_shp()

  # Handle no rows/columns (replace with NULL)
  is_empty <- nrow(hms_smoke) == 0 | ncol(hms_smoke) == 0
  if (is_empty) {
    return(NULL)
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

read_hms_shp <- function(
  shp_path,
  date_fmt = "%Y%j %H%M",
  date_tz = "UTC",
  date_cols = c("Start", "End"),
  density_levels = c("Heavy", "Medium", "Light")
) {
  sf::read_sf(shp_path) |>
    # Fix types
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(date_cols),
        \(x) x |> lubridate::as_datetime(format = date_fmt, tz = date_tz)
      ),
      Density = .data$Density |> factor(levels = density_levels),
    ) |>
    # Combine into multipolygons by period/density
    combine_polygons(
      .by = dplyr::all_of(c("Density", date_cols)),
      Satellite = .data$Satellite |> unique() |> paste(collapse = " + ")
    ) |>
    # Remove overlap of polygons so opacity works properly
    dplyr::arrange(
      dplyr::pick(dplyr::all_of(date_cols)),
      dplyr::desc(.data$Density)
    ) |>
    dplyr::group_by(dplyr::pick(dplyr::all_of(date_cols))) |>
    remove_polygon_overlap()
}

combine_polygons <- function(polygon_data, .by = NULL, ...) {
  polygon_data |>
    dplyr::filter(!sf::st_is_empty(.data$geometry)) |>
    sf::st_make_valid() |>
    dplyr::summarise(
      .by = rlang::enquo(.by),
      geometry = .data$geometry |> sf::st_union(),
      ...
    )
}

remove_polygon_overlap <- function(polygon_data, equal_area_crs = 3857) {
  polygon_data |>
    dplyr::group_modify(
      \(group_data, ...) {
        group_data |>
          sf::st_transform(equal_area_crs) |>
          sf::st_difference() |>
          sf::st_transform(sf::st_crs(group_data))
      }
    ) |> 
    dplyr::ungroup()
}
