#' Add multiple basemaps to a leaflet map
#'
#' Loop through provided leaflet provider names (see [leaflet::providers]) and add them to the map.
#' The layer group will match the names of `base_maps`, or to the values of `base_maps` if names are not provided.
#' The first element of `base_maps` will be used as the default basemap.
#'
#' @param map A leaflet map object
#' @param base_maps A character vector of base maps to add.
#'   Each element of the list should be found in [leaflet::providers].
#'   If names are provided, they will be used as the layer group names.
#'   If names are not provided, the values of `base_maps` will be used as the layer group names.
#'
#' @return A leaflet map object with the base maps added.
#'
#' @export
#'
#' @examples
#' library(leaflet)
#'
#' map <- leaflet()
#' base_maps <- c(
#'   "Light Theme" = providers$OpenStreetMap,
#'   "Terrain" = providers$Stamen.Terrain
#' )
#' map |> add_base_maps(base_maps)
add_base_maps <- function(map, base_maps) {
  stopifnot("leaflet" %in% class(map))
  stopifnot(is.character(base_maps), length(base_maps) > 0)

  # Handle case where names are not provided
  if (is.null(names(base_maps))) {
    names(base_maps) <- base_maps
  }

  # Loop through and add base maps
  for (name in names(base_maps)) {
    map <- map |>
      leaflet::addProviderTiles(
        provider = base_maps[[name]],
        group = name
      ) |>
      # Convert errors to warnings
      handyr::on_error(.return = map, .warn = TRUE)
  }

  # Insert base layers into basemap control (create control if needed)
  map <- map |>
    append_to_layer_contol(base_groups = names(base_maps))

  return(map)
}

append_to_layer_contol <- function(
  map,
  base_groups = character(0),
  layer_groups = character(0),
  ...
) {
  # Determine if control is already present
  is_control_call <- map$x$calls |>
    sapply(\(x) x$method) ==
    "addLayersControl"
  has_control <- any(is_control_call)

  if (has_control) {
    # Get existing control bases/layers (group names)
    existing_bases <- map$x$calls[is_control_call][[1]]$args[[1]]
    existing_layers <- map$x$calls[is_control_call][[1]]$args[[2]]

    # Append (unique) added bases/layers, drop duplicates
    map$x$calls[is_control_call][[1]]$args[[1]] <- c(
      existing_bases,
      base_groups
    ) |>
      unique()
    map$x$calls[is_control_call][[1]]$args[[2]] <- c(
      existing_layers,
      layer_groups
    ) |>
      unique()
  } else {
    # Otherwise, create new control
    map <- map |>
      leaflet::addLayersControl(
        baseGroups = base_groups,
        overlayGroups = layer_groups,
        ...
      )
  }
  return(map)
}
