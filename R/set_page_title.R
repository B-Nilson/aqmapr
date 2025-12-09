#' Set the page title of a Leaflet map
#'
#' @description
#' The page title is set by appending a `<title></title>` tag to the page `<head></head>`.
#' This title is visible in the browser tab when the map is saved to an HTML file.
#'
#' @param map A Leaflet map object
#' @param page_title The title of the map.
#'   Default is `NULL` (no title set).
#' @return A Leaflet map with the title set if `page_title` is not `NULL`, otherwise the original map is returned.
#' @export
set_page_title <- function(map, page_title = NULL) {
  if (is.null(page_title)) {
    return(map)
  }
  stopifnot("leaflet" %in% class(map))
  stopifnot(is.character(page_title), length(page_title) == 1)

  title_tag <- "<title>%s</title>\n" |> sprintf(page_title)
  new_dependency <- htmltools::htmlDependency(
    # Set tab title text
    head = title_tag,
    version = "0",
    name = "page_title",
    src = ""
  )

  map$dependencies <- map$dependencies |>
    append(list(new_dependency))
  return(map)
}
