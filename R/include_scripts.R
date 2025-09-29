#' Include a css/js file in a Leaflet map header
#'
#' Allows for including references to custom css/js in a Leaflet html widget
#' (see [leaflet::leaflet] and [htmlwidgets::prependContent]).
#' `paths` must be relative to the html file containing the map and be accessible by the html file.
#'
#' @param map A leaflet map object
#' @param paths The path(s) to the script file(s), relative to the html file containing the map.
#' @param types The type(s) of script file(s), either "css" or "js"
#'   Must be a single character string or vector the same length as `paths`.
#'   Default is "js".
#'
#' @return A leaflet map with the script file(s) included in the page header
#' @export
#' @examples
#' library(leaflet)
#' library(aqmapr)
#'
#' leaflet() |>
#'   add_base_maps(base_maps = "OpenStreetMap") |>
#'   include_scripts(
#'     paths = system.file("/js/map_layers.js", package = "aqmapr"),
#'     types = "js"
#'   )
include_scripts <- function(map, paths, types = "js") {
  stopifnot("leaflet" %in% class(map))
  stopifnot(
    is.character(paths),
    length(paths) > 0,
    length(paths) == length(types) | length(types) == 1
  )
  stopifnot(is.character(types), all(types %in% c("css", "js")))

  if (length(types) == 1) {
    types <- rep(types, length(paths))
  }
  # Loop through and add scripts to map header
  for (i in seq_along(paths)) {
    path <- paths[i]
    type <- types[i]

    # Build html for inserting script into header
    if (type == "css") {
      script_tag <- htmltools::tags$link(href = path, rel = "stylesheet")
    } else {
      script_tag <- htmltools::tags$script(src = path)
    }

    # Add to map
    map <- map |>
      htmlwidgets::prependContent(htmltools::tags$head(script_tag))
  }
  return(map)
}
