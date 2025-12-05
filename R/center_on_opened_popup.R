#' Center map on popup when opened
#'
#' @param map A leaflet map object
#' @param ... Additional arguments to pass to [include_scripts()]
#' @export
#' @return A leaflet map object with a centered popup
center_on_opened_popup <- function(map, ...) {
  stopifnot("leaflet" %in% class(map))

  js_file <- "js/center_on_popup.js" |>
    system.file(package = "aqmapr")
  map |>
    include_scripts(paths = js_file, ...)
}
