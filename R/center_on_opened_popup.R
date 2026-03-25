#' Center map on popup when opened
#'
#' @param map A leaflet map object
#' @param ... Additional arguments to pass to [include_scripts()]
#' @export
#' @return A leaflet map object with JS to center the map on popups when opened
center_on_opened_popup <- function(map, ...) {
  stopifnot("leaflet" %in% class(map))

  js_file <- "js/center_on_popup.js" |>
    system.file(package = "aqmapr")
  on_popup_open_js <- "(e) => { requestAnimationFrame(() => { center_on_popup(_map_global) }) }"
  map |>
    include_scripts(paths = js_file, ...) |>
    htmlwidgets::onRender(
      "(el, x) => { _map_global = this.getMap(); _map_global.on('popupopen', %s); }" |>
        sprintf(on_popup_open_js)
    )
}
