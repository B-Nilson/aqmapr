#' Simplify making a leaflet map
#'
#' @export
#' @param base_maps (Optional).
#'   A character vector named basemaps from [leaflet::providers] to add to the map.
#'   Names will be used in the control menu for the basemaps.
#'   Default is a nice light and dark open data theme.
#' @param point_layers,polygon_layers,wms_layers (Optional).
#'   A list of 1 or more `PointLayer`/`PolygonLayer`/`WMSLayer` objects (created with [PointLayer()]/[PolygonLayer()]/[WMSLayer()]) to be added to the map.
#'   Default is an empty list (no points/polygons/WMS layers added).
#' @param track_map_state (Optional). If TRUE, the map state will be tracked and saved in the URL when the map is saved to an HTML file.
#'   Default is TRUE.
#' @param include_timestamp (Optional).
#'   If TRUE, the current timestamp (browser time) will be included in a bottom left leaflet control.
#'   If a single POSIXct object is passed, it will be used as the timestamp instead of the current time.
#'   Default is FALSE (no timestamp added).
#' @param as_reference (Optional). If TRUE, js/css will be referenced in the map header. If FALSE, the js/css will be embeded directly in the map.
#'   Requires local server to be running (see [start_server()]), or the js and css files need to be hosted in "/js" and "/css" respectively relative to the html file.
#'   Run `system.file("js", package = "aqmapr")`/`system.file("css", package = "aqmapr")` to find the location of the js/css files respectively.
#'   Default is FALSE.
#' @return A leaflet map object.
#' @importFrom rlang exec !!!
#' @examples
#'
#' colour_pal <- leaflet::colorFactor(
#'   "viridis",
#'   domain = levels(canada_communities$type),
#'   ordered = TRUE,
#'   reverse = TRUE
#' )
#' point_layers <- list(PointLayer(
#'   group = "Communities",
#'   data = canada_communities,
#'   fill_palette = colour_pal,
#'   fill = ~type,
#'     label = ~ paste("Name: ", name, "<br/>", "Type: ", type) |>
#'       lapply(htmltools::HTML)
#' ))
#'
#' canadian_provinces <- load_canadian_provinces()
#' make_leaflet_map(
#'   polygon_layers = list(PolygonLayer(
#'     group = "Provinces",
#'     data = canadian_provinces,
#'     fill = "black",
#'     opacity = 0.1,
#'     label = ~name
#'   ))
#' )
make_leaflet_map <- function(
  base_maps = c(
    "Light Theme" = "OpenStreetMap",
    "Dark Theme" = "CartoDB.DarkMatter"
  ),
  point_layers = list(),
  polygon_layers = list(),
  wms_layers = list(),
  track_map_state = TRUE,
  include_timestamp = FALSE,
  as_reference = FALSE
) {
  stopifnot(
    is.character(base_maps),
    length(names(base_maps)) == length(base_maps)
  )
  stopifnot(identical("list", class(point_layers)))
  stopifnot(identical("list", class(polygon_layers)))
  stopifnot(identical("list", class(wms_layers)))
  stopifnot(is.logical(track_map_state), length(track_map_state) == 1)
  stopifnot(is.logical(as_reference), length(as_reference) == 1)
  stopifnot(
    is.logical(include_timestamp) | lubridate::is.POSIXct(include_timestamp),
    length(include_timestamp) == 1
  )

  # Make basemap
  base_map <- leaflet::leaflet() |>
    add_base_maps(base_maps = base_maps) |>
    # Cache provider tiles for faster reload times
    leaflet.extras::enableTileCaching()

  # Define map layers/defaults
  layer_names <- list(
    base = names(base_maps),
    data = point_layers |>
      sapply(\(x) x@group) |>
      c(polygon_layers |> sapply(\(x) x@group)) |>
      c(unname(wms_layers) |> sapply(\(x) x@group)),
    is_default = point_layers |>
      sapply(\(x) x@display_by_default[1]) |>
      c(polygon_layers |> sapply(\(x) x@display_by_default[1])) |>
      c(unname(wms_layers) |> sapply(\(x) x@display_by_default[1])) |>
      unlist()
  )
  if (any(layer_names$is_default)) {
    layer_names$defaults <- layer_names$data[which(layer_names$is_default)]
  } else {
    layer_names$defaults <- list()
  }
  layers_quoted <- layer_names[1:2] |>
    c(list(default = layer_names$defaults)) |>
    lapply(\(x) {
      paste0("'", x |> stringr::str_replace("'", "\\'"), "'") |>
        paste(collapse = ", ")
    })

  define_layers_js <- "function(el, x) { 
  _layers = {\"base\": [%s], \"data\": [%s] };
  _default_layers = { \"data\": [%s], \"base\": '%s' }; 
}" |>
    sprintf(
      layers_quoted$base,
      layers_quoted$data,
      layers_quoted$default,
      names(base_maps)[1]
    )
  base_map <- base_map |>
    htmlwidgets::onRender(define_layers_js)

  # Add a timestamp to bottom left if desired
  if (include_timestamp) {
    if (is.logical(include_timestamp)) {
      include_timestamp <- Sys.time()
    }
    base_map <- base_map |>
      add_map_timestamp(
        timestamp = include_timestamp,
        as_reference = as_reference
      )
  }

  # Add point layers as needed
  for (layer in point_layers) {
    base_map <- base_map |>
      add_to_map(layer = layer)
  }

  # Add polygon layers as needed
  for (layer in polygon_layers) {
    base_map <- base_map |>
      add_to_map(layer = layer)
  }

  # Add wms layers as needed
  if (length(wms_layers)) {
    base_map <- base_map |>
      add_wms_layers(wms_layers = wms_layers)
  }

  # Use leaflet.extras::addHash() + custom js
  # to track map location/layers/basemap
  if (track_map_state) {
    base_map <- base_map |>
      track_map_state(as_reference = as_reference)
  }

  return(base_map)
}

add_fill_legend <- function(
  map,
  data,
  fillColor,
  palette,
  group = NULL,
  title = group,
  opacity = 0.5,
  position = "bottomleft",
  na_label = "No Data"
) {
  # Handle NULL values
  if (is.null(position)) {
    position <- "bottomleft"
  }
  if (is.null(opacity)) {
    opacity <- 0.5
  }

  fill_values <- data |>
    dplyr::pull(!!rlang::as_quosure(fillColor))

  map |>
    leaflet::addLegend(
      title = title,
      group = group,
      pal = palette,
      values = fill_values,
      position = position,
      opacity = opacity,
      na.label = "No Data"
    )
}
