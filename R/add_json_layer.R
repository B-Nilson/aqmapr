#' Add a fetched or embeded GeoJSON layer to a Leaflet map
#'
#' @description
#' Adds a GeoJSON layer to a Leaflet map either as a reference to the file or embeded data.
#' [leaflet::addGeoJSON()] is used to add the layer as embeded data if `as_reference` is FALSE.
#' If `as_reference` is TRUE, a reference to the GeoJSON file is added to the page, such that the data can be fetched and added to the map on page load.
#' @param map A Leaflet map object
#' @param json_url A character string pointing to the URL of the GeoJSON data
#' @param layer_id,group (Optional).
#'   A character string of the layer id or group name to add to the layer.
#'   Default is NULL (no ID/group).
#' @param options (Optional).
#'   A list of options to pass to the L.geoJSON() method (see \url{https://leafletjs.com/reference.html#geojson}).
#'   List names must match the names of the L.geoJSON() options.
#'   Useful options:
#'   - Line styles: stroke (FALSE/TRUE), color, weight (pixels), opacity (0-1), \href{https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stroke-dasharray}{dashArray}
#'   - Fill styles: fill (FALSE/TRUE), fillColor, fillOpacity (0-1)
#'   - Other: bubblingMouseEvents (FALSE/TRUE), className
#'   Default is an empty list.
#' @param as_reference (Optional).
#'   If TRUE, the GeoJSON data will be fetched from the url and added to the map on page load.
#'   If FALSE, the GeoJSON data will be added as embeded data to the map.
#'   Default is FALSE.
#' @param add_to_layer_control (Optional).
#'   If TRUE, `group` must be provided and the layer will be added to the map's layer control under the group.
#'   Default is TRUE if `group` is provided, FALSE otherwise.
#' @export
#' @examples
#' library(leaflet)
#' library(aqmapr)
#'
#' geojson_url <- "https://raw.githubusercontent.com/B-Nilson/aqmapr/refs/heads/main/inst/extdata/example.geojson"
#'
#' # Add as embeded data (normal R leaflet method)
#' leaflet() |>
#'   add_base_maps(base_maps = "OpenStreetMap") |>
#'   add_geojson_layer(json_url = geojson_url, as_reference = FALSE)
#'
#' # Add as reference (fetch on page load)
#' leaflet() |>
#'   add_base_maps(base_maps = "OpenStreetMap") |>
#'   add_geojson_layer(json_url = geojson_url, as_reference = TRUE)
add_geojson_layer <- function(
  map,
  json_url,
  layer_id = NULL,
  group = NULL,
  options = list(),
  option_columns = list(
    iconUrl = "iconUrl",
    pane = "pane",
    zIndexOffset = "zIndexOffset",
    iconSize = "iconSize",
    label = "label",
    popup = "popup"
  ),
  as_reference = FALSE,
  display_on_load = TRUE,
  add_to_layer_control = !is.null(group)
) {
  if (identical(layer_id, character(0))) {
    layer_id <- NULL
  }
  stopifnot("leaflet" %in% class(map))
  stopifnot(is.character(json_url), length(json_url) == 1)
  stopifnot(
    is.null(layer_id) | (is.character(layer_id) & length(layer_id) == 1)
  )
  stopifnot(is.null(group) | (is.character(group) & length(group) == 1))
  stopifnot(is.list(options))
  stopifnot(is.logical(as_reference), length(as_reference) == 1)
  stopifnot(is.logical(add_to_layer_control), length(add_to_layer_control) == 1)
  stopifnot(!(add_to_layer_control & is.null(group)))

  # Path to the js file with method
  js_path <- system.file("js", package = "aqmapr") |>
    file.path("add_json_layer.js")

  if (!as_reference) {
    json_data <- readLines(url(json_url), warn = FALSE) |>
      paste(collapse = "\n")
    map <- map |>
      leaflet::addGeoJSON(
        geojson = json_data,
        layerId = layer_id,
        group = group,
        options = options
      )
  } else {
    map <- map |>
      leaflet::invokeMethod(
        data = NULL,
        method = "addJsonPointerLayer",
        json_url,
        layer_id,
        group,
        options,
        display_on_load,
        option_columns
      ) |>
      # Include js file inline in the header
      include_scripts(paths = js_path, as_reference = FALSE) |>
      # Define _map variable on page render
      htmlwidgets::onRender("function(el, x) { _map = this; }")
  }
  if (add_to_layer_control) {
    map <- map |>
      append_to_layer_control(layer_groups = group)
  }
  return(map)
}
