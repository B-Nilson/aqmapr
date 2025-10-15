#' Make a leaflet map with a base map and optional point data
#'
#' @export
#' @param base_maps A character vector of length 2, the first element is the light theme and the second element is the dark theme.
#' @param point_data A data frame of points to be added to the map.
#' @param point_options A list of options for the points, see \code{\link{addMarkers}} for details.
#' @return A leaflet map object.
#' @importFrom rlang exec !!!
make_leaflet_map <- function(
  base_maps = c(
    "Light Theme" = "OpenStreetMap",
    "Dark Theme" = "CartoDB.DarkMatter"
  ),
  point_data = NULL,
  point_options = list(
    radius = 5,
    weight = 1,
    color = "black",
    fillColor = "#3388ff",
    fillOpacity = 0.8,
    opacity = 1
  ),
  track_map_state = TRUE
) {
  base_map <- leaflet::leaflet() |>
    add_base_maps(base_maps = base_maps) |>
    # Cache provider tiles for faster reload times
    leaflet.extras::enableTileCaching()

  if (!is.null(point_data)) {
    base_map <- base_map |>
      rlang::exec(
        .fn = leaflet::addCircleMarkers,
        map = _,
        data = point_data,
        !!!point_options
      ) |>
      leaflet::addLayersControl(
        overlayGroups = layers$all,
        baseGroups = names(base_map_providers),
        position = control_pos$layers,
        options = leaflet::layersControlOptions(collapsed = TRUE)
      )
  }

  if (track_map_state) {
    base_map <- base_map |> 
    # Use leaflet.extras::addHash() + custom js
    # to track map location/layers/basemap
    track_map_state(as_reference = FALSE) 
  }
  return(base_map)
}
