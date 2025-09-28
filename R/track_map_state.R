track_map_state <- function(map) {
  js_paths <- c(
    "/js/constants.js",
    "/js/handlers.js",
    "/js/map_layers.js",
    "/js/track_map_state.js"
  )
  map |>
    leaflet.extras::addHash() |> 
    include_scripts(paths = js_paths, types = "js") |> 
    htmlwidgets::onRender("handle_page_render")
}
