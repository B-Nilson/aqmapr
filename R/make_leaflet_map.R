#' Simplify making a leaflet map
#'
#' @export
#' @param base_maps (Optional).
#'   A character vector named basemaps from [leaflet::providers] to add to the map.
#'   Names will be used in the control menu for the basemaps.
#'   Default is a nice light and dark open data theme.
#' @param point_data (Optional).
#'   A named list of 1 or more `sf` data frames of points to be added to the map.
#'   Names will be used in the control menu for the layers.
#'   Default is an empty list (no point layers added).
#' @param point_options (Optional). A list of options for the points, names must be present in arguments of [leaflet::addCircleMarkers()] OR
#'   must all be in `names(point_data)` if individual options for each layer are desired.
#'   You can use `~column_name` to pass a column from `point_data` as an option (i.e. `label = ~name`).
#'   Default is objectivley better options for point data, applied to all layers.
#' @param track_map_state (Optional). If TRUE, the map state will be tracked and saved in the URL when the map is saved to an HTML file.
#'   Default is TRUE.
#' @param as_reference (Optional). If TRUE, js/css will be referenced in the map header. If FALSE, the js/css will be embeded directly in the map.
#'   Requires local server to be running (see [start_server()]), or the js and css files need to be hosted in "/js" and "/css" respectively relative to the html file.
#'   Run `system.file("js", package = "aqmapr")`/`system.file("css", package = "aqmapr")` to find the location of the js/css files respectively.
#'   Default is FALSE.
#' @return A leaflet map object.
#' @importFrom rlang exec !!!
#' @examples
#' 
#' colour_pal <- "viridis" |> 
#'   leaflet::colorFactor(domain = canada_communities$type, ordered = TRUE)
#' make_leaflet_map(
#'   point_data = list("Communities" = canada_communities),
#'   point_options = list(
#'     radius = 3,
#'     weight = 1,
#'     color = "black",
#'     fillColor = ~colour_pal(name),
#'     fillOpacity = 0.8,
#'     opacity = 1,
#'     label = ~paste("Name: ", name, "<br/>", "Type: ", type) |> lapply(htmltools::HTML)
#'   )
#' ) |> 
#'   leaflet::addLegend(
#'     pal = colour_pal, 
#'     values = levels(canada_communities$type) |> 
#'         factor(levels = levels(canada_communities$type))
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
  track_map_state = TRUE,
  as_reference = FALSE
) {
  stopifnot(is.character(base_maps), length(names(base_maps)) == length(base_maps))
  stopifnot(identical("list", class(point_data)))
  stopifnot(identical("list", class(point_options)))
  stopifnot(is.logical(track_map_state), length(track_map_state) == 1)
  stopifnot(is.logical(as_reference), length(as_reference) == 1)

  # Check if each layer has its own options
  use_indiv_options <- all(names(point_options) %in% names(point_data))

  base_map <- leaflet::leaflet() |>
    add_base_maps(base_maps = base_maps) |>
    # Cache provider tiles for faster reload times
    leaflet.extras::enableTileCaching()

  if (length(point_data) > 0) {
    for (group in names(point_data)) {
      if (use_indiv_options) {
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

  # Use leaflet.extras::addHash() + custom js
  # to track map location/layers/basemap
  if (track_map_state) {
    base_map <- base_map |>
      track_map_state(as_reference = as_reference)
  }
  return(base_map)
}
