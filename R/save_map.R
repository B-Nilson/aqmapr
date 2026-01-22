#' Save a leaflet map to a file
#'
#' @description
#' Largely a wrapper around [mapview::mapshot], allowing for saving a leaflet map to a file.
#'
#' @param map A leaflet map
#' @param save_to The file path to save the map to.
#'   File extension must be one of ".html", ".png", ".pdf", or ".jpeg".
#'   Default is `"./index.html"`.
#' @param self_contained Should the map be written to a standalone file (`TRUE`) or a file linking to dependencies written to a folder named after `library_dir` (`FALSE`).
#'   Ignored if `save_to` is not a ".html" file (as those are always self-contained).
#'   Default is `FALSE`.
#' @param library_dir The directory to write dependencies to when `self_contained` is `FALSE`.
#'   Default is `NULL`, in which case dependencies are written to a folder named after `save_to` + "_files".
#' @param encoding The encoding to use when writing the file.
#'   Default is `"UTF-8"`.
#'
#' @param ... Additional arguments to pass to [mapview::mapshot] (which calls [webshot::webshot] for non-HTML files, and [htmlwidgets::saveWidget] for HTML).
#' @export
save_map <- function(
  map,
  save_to = "./index.html",
  self_contained = FALSE,
  library_dir = NULL,
  encoding = "UTF-8",
  ...
) {
  rlang::check_installed("webshot") # TODO: remove once PR #516 for mapview is merged
  stopifnot("leaflet" %in% class(map))
  stopifnot(
    is.character(save_to),
    tools::file_ext(save_to) %in% c("html", "png", "pdf", "jpeg"),
    length(save_to) == 1
  )
  stopifnot(is.logical(self_contained), length(self_contained) == 1)

  # Translate to mapshot arguments
  args <- list(
    x = map,
    selfcontained = self_contained,
    libdir = library_dir,
    encoding = encoding,
    ...
  )

  # Handle different file types
  if (tools::file_ext(save_to) == "html") {
    args$url <- save_to
  } else {
    args$file <- save_to
  }

  # Call mapshot
  do.call(mapview::mapshot, args)
}
