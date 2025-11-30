#' Include a css reference to a font in a Leaflet map header
#'
#' Allows for including references to custom fonts hosted on platforms like \href{https://fonts.google.com/}{https://fonts.google.com/}.
#' Note, by default this only make the font available to the page, you still need to use the font in your css or set `force_display` to TRUE.
#'
#' @param map A leaflet map object
#' @param font_urls The url(s) of the font(s) to include.
#'   If using google fonts, go to https://fonts.google.com/specimen/FONT_NAME (or search for the font from there), click "get font", then "get embed code", then "@import". The url should be within `url()` there.
#'   If `force_display` is TRUE, the name(s) of the url(s) must be set to the names of the font(s) (i.e. `font_urls = c("Inter" = "...")`).
#' @param force_display (Optional).
#'   If FALSE, the font(s) will be included in the page header and will need to be referenced in your css.
#'   If TRUE, only one font (which must be named) can be included and it will be force applied to all elements on the page.
#'   Note, this may be slow for many elements.
#'   Default is FALSE.
#' @return A leaflet map with the font(s) included in the page header
#' @export
include_font <- function(map, font_urls, force_display = FALSE) {
  stopifnot("leaflet" %in% class(map))
  stopifnot(is.character(font_urls), length(font_urls) > 0)
  stopifnot(
    is.logical(force_display),
    length(force_display) == 1,
    !force_display | (!is.null(names(font_urls)) & length(font_urls) == 1)
  )
  if (force_display) {
    force_code <- paste0(
      "\n*:not(i):not(.fa){\n\tfont-family:",
      names(font_urls),
      " !important;\n}\n"
    )
  } else {
    force_code <- ""
  }

  map |>
    include_scripts(
      texts = paste0("@import url(", font_urls, ");", force_code),
      types = "css",
      as_reference = FALSE
    )
}
