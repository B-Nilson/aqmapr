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
#' @param timeout Number of seconds to allow for downloading the shapefile zip.
#'   Defaults to `max(getOption("timeout"), 300)`. Increase this if downloads
#'   fail on a slow connection.
#' @param cache_refresh_hours How old (in hours) the cached copy of the current
#'   day's file can be before it is re-downloaded. NOAA updates today's HMS
#'   smoke file throughout the day, so an early-morning download quickly goes
#'   stale. Past files never change and stay cached indefinitely. Defaults to 1.
#'   Use `Inf` to never refresh the current day's file, or `0` to always
#'   re-download it.
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
  quiet = FALSE,
  cache = TRUE,
  timeout = max(getOption("timeout"), 300),
  cache_refresh_hours = 1
) {
  stopifnot(is.numeric(timeout), length(timeout) == 1, timeout > 0)
  stopifnot(
    is.numeric(cache_refresh_hours),
    length(cache_refresh_hours) == 1,
    cache_refresh_hours >= 0
  )

  desired_cols <- c(
    satellite = "Satellite",
    "period",
    density = "Density"
  )

  select_time <- select_time |> lubridate::with_tz("America/Vancouver")
  shape_date <- select_time |> format("%Y%m%d")
  shape_month <- select_time |> format("%Y/%m")
  # NOAA updates the current day's smoke file throughout the day, while past
  # files never change
  is_todays <- shape_date == format(Sys.time(), "%Y%m%d", tz = "America/Vancouver")
  date_cols <- c("Start", "End")

  # Build url to desired zip file
  source_url <- "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile"
  zip_url <- "%s/%s/hms_smoke%s.zip" |>
    sprintf(source_url, shape_month, shape_date)

  # Download, unzip, read
  local_path <- "%s/hms_%s_shp.zip" |>
    sprintf(data_dir, shape_date)
  # Extracted shapefiles for this date (used to skip re-unzipping on cached calls)
  extracted_pattern <- sprintf("^hms_smoke%s.*\\.shp$", shape_date)
  # Refresh the current day's cached file once it goes stale. Also drop the
  # extracted shapefile: NOAA appends new smoke analysis to today's file, so
  # the old extraction must not be reused after a refresh.
  if (cache_file_stale(local_path, is_todays, cache, cache_refresh_hours)) {
    unlink(local_path)
    unlink(list.files(data_dir, pattern = extracted_pattern, full.names = TRUE))
  }
  old_timeout <- getOption("timeout")
  options(timeout = timeout)
  on.exit(options(timeout = old_timeout), add = TRUE)

  # If this date's shapefile is already extracted, reuse it without
  # downloading or unzipping again
  shp_paths <- list.files(data_dir, pattern = extracted_pattern, full.names = TRUE)
  if (!cache || length(shp_paths) == 0) {
    shp_paths <- tryCatch(
      zip_url |>
        handyr::get_and_unzip(
          local_path = local_path,
          unzip_dir = data_dir,
          cache = cache,
          quiet = quiet
        ) |>
        stringr::str_subset(pattern = ".*\\.shp$"),
      error = function(e) {
        # Drop any partial download so a cached retry re-downloads cleanly
        unlink(local_path)
        stop(
          sprintf(
            "Failed to get HMS smoke polygons for %s: %s.",
            format(select_time, "%Y-%m-%d"),
            conditionMessage(e)
          ),
          call. = FALSE
        )
      }
    )
  }
  hms_smoke <- read_hms_shp(shp_paths, date_cols = date_cols)

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
      .by = {{ .by }},
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
    dplyr::ungroup() |>
    sf::st_sf()
}

# Should a cached download be re-fetched because it has gone stale?
cache_file_stale <- function(local_path, is_todays, cache, cache_refresh_hours) {
  is_todays && cache && file.exists(local_path) &&
    get_file_age(local_path) > lubridate::dhours(cache_refresh_hours)
}
