#' Add multiple basemap providers to a Leaflet map
#'
#' Loop through desired Leaflet provider names (see [leaflet::providers]) and add them to the map.
#' The layer group will match the names of `base_maps`, or to the values of `base_maps` if names are not provided.
#' The first element of `base_maps` will be used as the default basemap.
#'
#' @param map A leaflet map object
#' @param base_maps A character vector of base maps to add.
#'   Each value must be found in [leaflet::providers].
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
  stopifnot(
    is.character(base_maps),
    length(base_maps) > 0,
    all(base_maps %in% leaflet::providers)
  )

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
    append_to_layer_control(base_groups = names(base_maps))

  return(map)
}
