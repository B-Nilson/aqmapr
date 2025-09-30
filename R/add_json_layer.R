#' Add a fetched GeoJSON layer to a Leaflet map
#'
#' @description
#' Adds a GeoJSON layer to a Leaflet map either as a reference to the file or embeded data.
#' [leaflet::addGeoJSON()] is used to add the layer as embeded data if `as_reference` is FALSE.
#' If `as_reference` is TRUE, a reference to the GeoJSON file is added to the page, such that the data can be fetched and added to the map on page load.
#' @param map A Leaflet map object
#' @param json_url A character string pointing to the URL of the GeoJSON data
#' @param layer_id (Optional). A character string of the layer id to add the layer to
#' @param options (Optional). A list of options to pass to the L.geoJSON() method
#' @param as_reference (Optional).
#'   If TRUE, the GeoJSON data will be fetched from the url and added to the map on page load.
#'   If FALSE, the GeoJSON data will be added as embeded data to the map.
#' @export
#' @examples
#' library(leaflet)
#' library(aqmapr)
#'
#' leaflet() |>
#'   add_base_maps(base_maps = "OpenStreetMap") |>
#'   add_geojson_layer(json_url = "https://raw.githubusercontent.com/B-Nilson/aqmapr/refs/heads/main/inst/extdata/canadian_provinces.geojson")
add_geojson_layer <- function(
  map,
  json_url,
  layer_id = NULL,
  group = NULL,
  options = list(),
  as_reference = TRUE,
  add_to_layer_control = !is.null(group)
) {
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
        options
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
