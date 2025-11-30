#' Add a timestamp to the bottom left of a Leaflet map
#'
#' The timestamp is included in a leaflet control (bottom left by default)
#' and is displayed in the same timezone as the browser by default.
#' "Last updated: " is pre-pended by default, but can be changed with `prefix`.
#' The timestamp can be displayed in French by setting `en_francais` to TRUE.
#'
#' @param map A leaflet map object
#' @param timestamp A POSIXt object representing the timestamp to display.
#'   Default is the current time using `Sys.time()`.
#' @param prefix A string to prepend to the timestamp.
#'   Default is "Last updated: "
#'   OR "Dernière mise à jour: " if `en_francais` is TRUE
#' @param date_format A string specifying the format of the timestamp.
#'   The 3 letter abbreviation for the timezone is appended automatically.
#'   Default is "%Y %b %d %H:%M" (e.g. "2022 Jan 01 00:00").
#'   Or "%d %b %Y %H:%M" (e.g. "01 Jan 2022 00:00") if `en_francais` is TRUE.
#' @param use_browser_timezone If TRUE, the timestamp will be displayed in the same timezone as the browser.
#'   If FALSE, the timestamp will be displayed in the same timezone as the `timestamp` argument.
#'   Default is TRUE.
#' @param hover_text A string to display when the mouse hovers over the timestamp.
#'   Default is "" (no hover text).
#' @param position The position of the timestamp in the map.
#'   One of "topright", "topleft", "bottomright", "bottomleft".
#'   Default is "bottomleft".
#' @param remove_transparency If TRUE, the timestamp control will be displayed without transparency.
#'   Otherwise, the default leaflet control transparency of 80% will be used.
#'   Default is TRUE.
#' @param as_reference If TRUE, the path to the script file will be included in the src attribute of the script tag.
#'   If FALSE, the script will be embedded directly.
#'   Default is FALSE.
#' @param en_francais If TRUE, the timestamp will be displayed in French instead of English.
#'   Default is FALSE.
#'
#' @return A leaflet map with the timestamp added
#' @export
#' @examples
#' library(leaflet)
#' library(aqmapr)
#'
#' leaflet() |>
#'   add_base_maps(base_maps = "OpenStreetMap") |>
#'   add_map_timestamp(timestamp = as.POSIXct("2022-01-01 00:00:00", tz = "UTC"))
add_map_timestamp <- function(
  map,
  timestamp = Sys.time(),
  prefix = ifelse(en_francais, "Derni\u00E8re mise \u00E0 jour: ", "Last updated: "),
  date_format = ifelse(en_francais, '%d %b %Y %H:%M', "%Y %b %d %H:%M"), # + timezone
  use_browser_timezone = TRUE,
  hover_text = "",
  position = "bottomleft",
  remove_transparency = TRUE,
  as_reference = FALSE,
  en_francais = FALSE
) {
  stopifnot("leaflet" %in% class(map))
  stopifnot(lubridate::is.POSIXct(timestamp), length(timestamp) == 1)
  stopifnot(is.character(prefix), length(prefix) == 1)
  stopifnot(is.character(date_format), length(date_format) == 1)
  stopifnot(is.character(hover_text), length(hover_text) == 1)
  stopifnot(
    is.character(position),
    length(position) == 1,
    position %in% c("topright", "topleft", "bottomright", "bottomleft")
  )
  stopifnot(is.logical(remove_transparency), length(remove_transparency) == 1)
  stopifnot(is.logical(as_reference), length(as_reference) == 1)
  stopifnot(is.logical(en_francais), length(en_francais) == 1)

  js_path <- system.file("js/convert_utc_to_local.js", package = "aqmapr")
  hcharts_path <- "https://code.highcharts.com/highcharts.js"
  prefix <- gsub("'", "\\'", prefix) # Escape single quotes
  hover_text <- gsub("'", "\\'", hover_text) # Escape single quotes
  ts_placeholder <- timestamp |>
    lubridate::with_tz(tzone = "UTC") |>
    format("%Y-%m-%dT%H:%M:%SZ")

  map |>
    # Add the timestamp control - UTC placeholder will be replaced by JS
    leaflet::addControl(
      html = paste0('<big><strong>', prefix, ts_placeholder, '</strong></big>'),
      layerId = "map_timestamp",
      position = position
    ) |>
    # Include js file inline in the header
    include_scripts(paths = js_path, as_reference = as_reference) |>
    # Include highcharts reference in the header for R-like timestamp formatting
    include_scripts(paths = hcharts_path, as_reference = TRUE) |>
    # Define _map variable on page render
    htmlwidgets::onRender(
      "function(el, x) { format_map_timestamp('%s', %s, '%s', '%s', %s); }" |>
        sprintf(
          hover_text,
          tolower(remove_transparency),
          date_format,
          use_browser_timezone |>
            ifelse(yes = "browser", no = format(timestamp, "%Z")),
          tolower(en_francais)
        )
    )
}
