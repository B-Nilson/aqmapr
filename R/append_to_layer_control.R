#' Append a layer control to a leaflet map
#'
#' Append a layer control to a leaflet map, allowing users to toggle
#' layers on and off.
#'
#' @param map A leaflet map object
#' @param base_groups (Optional).
#'   A character vector of base layer group names which match the group names of base maps added with [add_base_maps()] or [leaflet::addProviderTiles()].
#'   These will be added to the layer control "baseGroups" list (see [leaflet::addLayersControl()]).
#'   Default is an empty character vector.
#' @param layer_groups (Optional).
#'   A character vector of layer group names which match the group names of layers added with [leaflet::addMarkers()], [leaflet::addPolygons()], etc.
#'   These will be added to the layer control "overlayGroups" list (see [leaflet::addLayersControl()]).
#'   Default is an empty character vector.
#' @param ... Additional arguments passed to leaflet::addLayersControl()
#'
#' @return A leaflet map object with the layer control appended
#' @export
#' @examples
#' library(leaflet)
#' library(aqmapr)
#' 
#' # Create basic map with layer control
#' map <- leaflet() |>
#'   add_base_maps(base_maps = "OpenStreetMap")
#' 
#' # Add a layer and include it in the exising layer control
#' map  |>
#'   leaflet::addMarkers(lat = -37.8136, lng = 144.9631, group = "test") |>
#'   append_to_layer_control(layer_groups = "test") 
append_to_layer_control <- function(
  map,
  base_groups = character(0),
  layer_groups = character(0),
  ...
) {
  stopifnot("leaflet" %in% class(map))
  stopifnot(is.character(base_groups))
  stopifnot(is.character(layer_groups))
  stopifnot(length(base_groups) > 0 | length(layer_groups) > 0)

  # Determine if control is already present
  is_control_call <- map$x$calls |>
    sapply(\(x) x$method == "addLayersControl")
  has_control <- any(is_control_call)

  # Return map with new control if none present
  if (!has_control) {
    map <- map |>
      leaflet::addLayersControl(
        baseGroups = base_groups,
        overlayGroups = layer_groups,
        ...
      )
    return(map)
  }

  # Otherwise, get existing control bases/layers (group names)
  existing_bases <- map$x$calls[is_control_call][[1]]$args[[1]]
  existing_layers <- map$x$calls[is_control_call][[1]]$args[[2]]

  # Append (unique) added bases/layers, drop duplicates
  map$x$calls[is_control_call][[1]]$args[[1]] <- existing_bases |>
    c(base_groups) |>
    unique()
  map$x$calls[is_control_call][[1]]$args[[2]] <- existing_layers |>
    c(layer_groups) |>
    unique()

  return(map)
}
