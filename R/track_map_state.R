#' Track active map layers and view in the URL
#'
#' Track map state (layers, center, zoom) and update URL accordingly.
#' On page load, check URL for state and set map accordingly.
#'
#' @param map A leaflet map object
#' @param js_dir The directory where custom js files are stored.
#'   This should not need to be changed.
#'   Default is the "js" directory in the `aqmapr` package.
#' @param js_endpoint The endpoint where custom js files can be accessed by the client. 
#'   Typically this is either a ambiorix/plumber endpoint or a static directory 
#'   (relative to the html file being served).
#'
#' @return A leaflet map object
#' @export
#' @examples
#' library(leaflet)
#' library(aqmapr)
#' 
#' leaflet() |>
#'   add_base_maps(base_maps = "OpenStreetMap") |>
#'   track_map_state()
track_map_state <- function(
  map,
  js_dir = system.file("js", package = "aqmapr"),
  js_endpoint = "/js"
) {
  stopifnot("leaflet" %in% class(map))
  stopifnot(length(js_dir) == 1, is.character(js_dir), dir.exists(js_dir))
  stopifnot(
    length(js_endpoint) == 1,
    is.character(js_endpoint),
    startsWith(js_endpoint, "/")
  )

  # Specify required js files
  js_files <- c(
    "on_render.js", # # in case not already loaded
    "map_layers.js", # # in case not already loaded
    "track_map_state/url_args.js",
    "track_map_state/handlers.js"
  )
  js_local <- file.path(js_dir, js_files)
  js_server <- file.path(js_endpoint, js_files)

  # Make sure the js files exist
  stopifnot(all(file.exists(js_local)))

  map |>
    # Track zoom/lat/lng in URL
    leaflet.extras::addHash() |>
    # Include js files to track active layers and base map
    include_scripts(paths = js_server, types = "js") |>
    # Start js on page render (incase not already done)
    htmlwidgets::onRender("handle_page_render")
}
