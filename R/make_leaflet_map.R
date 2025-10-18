#' Simplify making a leaflet map
#'
#' @export
#' @param base_maps (Optional).
#'   A character vector named basemaps from [leaflet::providers] to add to the map.
#'   Names will be used in the control menu for the basemaps.
#'   Default is a nice light and dark open data theme.
#' @param point_data,polygon_data (Optional).
#'   A named list of 1 or more `sf` data frames of points/polygons to be added to the map.
#'   Names will be used in the control menu for the layers.
#'   Default is an empty list (no points/polygons added).
#' @param point_options,polygon_options (Optional).
#'   A list of options for the points/polygons, names must be present in arguments of [leaflet::addCircleMarkers()]/[leaflet::addPolygons()] OR
#'   must all be in `names(point_data)`/`names(polygon_data)` if individual options for each layer are desired.
#'   You can use `~column_name` to pass a column from `point_data`/`polygon_data` as an option (i.e. `label = ~name`).
#'   Default is objectivley better options for point/polygon data, applied to all layers.
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
#' make_leaflet_map(
#'   point_data = list("Communities" = canada_communities),
#'   point_options = list(
#'     radius = 3,
#'     weight = 1,
#'     color = "black",
#'     fillColor = ~ colour_pal(type),
#'     fillOpacity = 0.8,
#'     opacity = 1,
#'     label = ~ paste("Name: ", name, "<br/>", "Type: ", type) |>
#'       lapply(htmltools::HTML)
#'   )
#' ) |>
#'   leaflet::addLegend(
#'     pal = colour_pal,
#'     values = unique(canada_communities$type) |> sort()
#'   )
#'
#' canadian_provinces <- load_canadian_provinces()
#'   make_leaflet_map(
#'     polygon_data = list("Provinces" = canadian_provinces),
#'     polygon_options = list(
#'       weight = 1,
#'       color = "black",
#'       fillColor = "black",
#'       fillOpacity = 0.1,
#'       opacity = 1,
#'       label = ~name
#'     )
#'   )
make_leaflet_map <- function(
  base_maps = c(
    "Light Theme" = "OpenStreetMap",
    "Dark Theme" = "CartoDB.DarkMatter"
  ),
  point_data = list(),
  point_options = list(
    radius = 3,
    weight = 1,
    color = "black",
    fillColor = "#808080",
    fillOpacity = 0.8,
    opacity = 1
  ),
  polygon_data = list(),
  polygon_options = list(
    weight = 2,
    color = "black",
    fillColor = "#808080",
    fillOpacity = 0.8,
    opacity = 1
  ),
  track_map_state = TRUE,
  include_timestamp = FALSE,
  as_reference = FALSE
) {
  stopifnot(
    is.character(base_maps),
    length(names(base_maps)) == length(base_maps)
  )
  stopifnot(identical("list", class(point_data)))
  stopifnot(identical("list", class(point_options)))
  stopifnot(identical("list", class(polygon_data)))
  stopifnot(identical("list", class(polygon_options)))
  stopifnot(is.logical(track_map_state), length(track_map_state) == 1)
  stopifnot(is.logical(as_reference), length(as_reference) == 1)
  stopifnot(
    is.logical(include_timestamp) | lubridate::is.POSIXct(include_timestamp),
    length(include_timestamp) == 1
  )

  # Check if each layer has its own options
  use_indiv_options <- list(
    points = all(names(point_options) %in% names(point_data)),
    polygons = all(names(polygon_options) %in% names(polygon_data))
  )

  base_map <- leaflet::leaflet() |>
    add_base_maps(base_maps = base_maps) |>
    # Cache provider tiles for faster reload times
    leaflet.extras::enableTileCaching()

  # Add point layers as needed
  if (length(point_data) > 0) {
    for (group in names(point_data)) {
      if (use_indiv_options$points) {
        p_options <- point_options[[group]]
      } else {
        p_options <- point_options
      }
      base_map <- rlang::exec(
        .fn = leaflet::addCircleMarkers,
        map = base_map,
        data = point_data[[group]],
        group = group,
        !!!p_options
      )
    }
    base_map <- base_map |>
      append_to_layer_control(
        layer_groups = names(point_data)
      )
  }

  # Add polygon layers as needed
  if (length(polygon_data) > 0) {
    for (group in names(polygon_data)) {
      if (use_indiv_options$polygons) {
        p_options <- polygon_options[[group]]
      } else {
        p_options <- polygon_options
      }
      base_map <- rlang::exec(
        .fn = leaflet::addPolygons,
        map = base_map,
        data = polygon_data[[group]],
        group = group,
        !!!p_options
      ) |>
        withr::with_package(package = "sf")
    }
    base_map <- base_map |>
      append_to_layer_control(
        layer_groups = names(polygon_data)
      )
  }

  # Use leaflet.extras::addHash() + custom js
  # to track map location/layers/basemap
  if (track_map_state) {
    base_map <- base_map |>
      track_map_state(as_reference = as_reference)
  }

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

  return(base_map)
}
