#' Add attribution to map
#'
#' This function appends text to the map layer attribution in the bottom right.
#'
#' @param map The map to add the attribution to.
#' @param ... The strings to concatenate to form the attribution. If nothing provided
#' @param sep The separator to use when concatenating the strings. The default
#' separator is " | ".
#' @return The map with the attribution added.
#' @export
#' @examples
#' library(aqmapr)
#' map <- make_leaflet_map()
#' map |>
#'   add_attribution(
#'     "Creator: <a href='mailto:Kg4Xo@example.com'>John Doe</a>",
#'     "Data source: <a href='https://example.com'>example.com</a>"
#' )
#'
add_attribution <- function(map, ..., sep = " | ") {
  attribution <- sep |> 
    paste0(paste(..., sep = sep, collapse = sep))
  no_attribution <- attribution == sep
  if (no_attribution) {
    return(map)
  }
  map |>
    leaflet::addTiles(
      urlTemplate = "",
      attribution = attribution
    )
}
