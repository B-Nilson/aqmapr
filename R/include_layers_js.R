#' Include js in a Leaflet map header with references to layers
#'
#' @inheritParams make_leaflet_map
#' @return A leaflet map with the script file(s) included in the page header
#' @export
#' @examples
#' library(leaflet)
#' library(aqmapr)
#'
#' leaflet() |>
#'   add_base_maps(base_maps = "OpenStreetMap") |>
#'   include_layers_js(
#'     base_maps = c("Light Theme" = "OpenStreetMap"),
#'     point_layers = list(),
#'     polygon_layers = list(),
#'     wms_layers = list()
#'   )
include_layers_js <- function(
  map,
  base_maps = c("Light Theme" = "OpenStreetMap"),
  point_layers = list(),
  polygon_layers = list(),
  wms_layers = list()
) {
  stopifnot(
    is.character(base_maps),
    length(base_maps) > 0,
    length(names(base_maps)) == length(base_maps)
  )
  # Define map layers/defaults
  layer_names <- list(
    base = names(base_maps),
    data = c(point_layers, polygon_layers, wms_layers) |>
      sapply(\(x) x@group) |>
      unname(),
    defaults = c(point_layers, polygon_layers, wms_layers) |>
      sapply(\(x) ifelse(x@display_by_default, x@group, NA)) |>
      unname() |>
      stats::na.omit()
  )
  layers_quoted <- layer_names |>
    lapply(\(x) {
      x_safe <- x |> stringr::str_replace_all("'", "\\'")
      "'%s'" |> sprintf(x_safe) |> paste(collapse = ", ")
    })
  define_layers_js <- '
    function(el, x) { 
      _layers = {"base": [%s], "data": [%s] };
      _default_layers = { "data": [%s], "base": "%s" }; 
    }' |>
    sprintf(
      layers_quoted$base,
      layers_quoted$data,
      layers_quoted$default,
      names(base_maps)[1] |> stringr::str_replace_all('"', '\\"')
    )
  map |>
    htmlwidgets::onRender(define_layers_js)
}