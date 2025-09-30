#' Add a fetched GeoJSON layer to a Leaflet map
#'
#' @description
#' Adds a reference GeoJSON layer to a Leaflet map. 
#' This differs from [leaflet::addPolygons()] etc in that 
#' the GeoJSON data is fetched from a URL on page load, instead of baked into the page.
#' @param map A Leaflet map object
#' @param json_url A character string pointing to the URL of the GeoJSON data
#' @param layer_id (Optional). A character string of the layer id to add the layer to
#' @param options (Optional). A list of options to pass to the L.geoJSON() method
#' @export
#' @examples
#' library(leaflet)
#' library(aqmapr)
#' 
#' leaflet() |>
#'   add_base_maps(base_maps = "OpenStreetMap") |>
#'   add_json_layer(json_url = "https://gist.githubusercontent.com/wavded/1200773/raw/e122cf709898c09758aecfef349964a8d73a83f3/sample.json")
add_json_layer <- function(
  map,
  json_url,
  layer_id = NULL,
  options = list()
) {
  stopifnot("leaflet" %in% class(map))
  stopifnot(is.character(json_url), length(json_url) == 1)
  stopifnot(is.null(layer_id) | (is.character(layer_id) &length(layer_id) == 1))
  stopifnot(is.list(options))

  # Path the js file with method
  js_path <- system.file("js", package = "aqmapr") |>
    file.path("add_json_layer.js")

  map |>
    leaflet::invokeMethod(
      data = NULL,
      method = "addJsonPointerLayer",
      json_url,
      layer_id,
      options
    ) |>
    # Include js file inline in the header
    include_scripts(paths = js_path, as_reference = FALSE) |>
    # Define _map variable on page render
    htmlwidgets::onRender("function(el, x) { _map = this; }")
}
