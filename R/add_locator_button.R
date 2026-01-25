add_locator_button <- function(map, hover_text = "Zoom to your location") {
  locate_button <- leaflet::easyButton(
    icon = "fa-crosshairs",
    title = hover_text,
    onClick = leaflet::JS("on_locator_click") # provided by js/zoom_to_location.js
  )
  js_path <- "js/zoom_to_location.js" |>
    system.file(package = "aqmapr")
  map |>
    include_scripts(paths = js_path) |>
    leaflet::addEasyButton(locate_button)
}
