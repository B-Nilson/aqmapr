#' Include a css/js file in a Leaflet map header
#'
#' Allows for including references to custom css/js in a Leaflet html widget
#' (see [leaflet::leaflet] and [htmlwidgets::prependContent]).
#' `paths` must be relative to the html file containing the map and be accessible by the html file.
#'
#' @param map A leaflet map object
#' @param texts,paths
#'   The individual script contents to be included (texts)
#'   or the path(s) to the script file(s) relative to the html file containing the map.
#'   One of `texts` or `paths` must be provided, but not both.
#'   If `texts` is provided, `types` must also be provided.
#' @param types The type(s) of script file(s), either "css" or "js"
#'   Must be a single character string or vector the same length as `paths`.
#'   Default is NULL, in which case the type will be inferred from the file extension.
#' @param as_reference (Optional).
#'   If TRUE, the path will be included in the src attribute of the script tag (js) or href attribute of the link tag (css).
#'   If FALSE, `texts` will be used, or the paths will be read in, and that will be included in the tags directly.
#'   Default is FALSE.
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
#'     paths = system.file("js/map_layers.js", package = "aqmapr"),
#'     types = "js",
#'     as_reference = FALSE
#'   )
include_scripts <- function(
  map,
  texts = NULL,
  paths = NULL,
  types = NULL,
  as_reference = FALSE
) {
  stopifnot("leaflet" %in% class(map))
  stopifnot(
    !is.null(texts) | !is.null(paths),
    is.null(texts) | is.null(paths)
  )
  stopifnot(!is.null(texts) & !as_reference | is.null(texts))
  stopifnot(!is.null(texts) & !is.null(types) | is.null(texts))
  stopifnot(is.null(texts) | is.character(texts) & length(texts) > 0)
  stopifnot(is.null(paths) | (is.character(paths) & length(paths) > 0))
  stopifnot(
    is.null(types) | (is.character(types) & all(types %in% c("css", "js")))
  )
  stopifnot(is.logical(as_reference), length(as_reference) == 1)

  # If types not specified, infer from file extensions
  if (is.null(types)) {
    types <- tools::file_ext(paths)
    stopifnot(all(types %in% c("css", "js")))
  }

  if (is.null(texts) & !as_reference) {
    inputs <- paths |>
      lapply(\(path) {
        path |>
          readLines() |>
          paste(collapse = "\n") |> 
          htmltools::HTML()
      })
  } else if (!is.null(paths)){
    inputs <- paths
  } else {
    inputs <- texts |> 
      lapply(htmltools::HTML)
  }

  # Ensure types is the same length as inputs
  if (length(types) == 1) {
    types <- rep(types, length(inputs))
  }

  # Loop through and add scripts to map header
  for (i in seq_along(inputs)) {
    input <- inputs[[i]]
    type <- types[i]

    # Build html for inserting script into header
    if (!as_reference) {
      if (type == "css") {
        script_tag <- input |>
          htmltools::tags$style(type = "text/css")
      } else {
        script_tag <- input |>
          htmltools::tags$script(type = "text/javascript")
      }
    } else {
      if (type == "css") {
        script_tag <- htmltools::tags$link(href = input, rel = "stylesheet")
      } else {
        script_tag <- htmltools::tags$script(src = input)
      }
    }

    # Add to map
    map <- map |>
      htmlwidgets::prependContent(htmltools::tags$head(script_tag))
  }
  return(map)
}
