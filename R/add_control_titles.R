#' Insert titles into a Leaflet layer control
#'
#' @description
#' Adds titles to differiate the basemap/overlays sections of a Leaflet layer control.
#'
#' @param base_title (Optional) The title to display in the base maps section of the layer control.
#'   Default is `"Basemaps"`.
#' @param layers_title (Optional) The title to display in the overlays section of the layer control.
#'   Default is `"Layers"`.
#' @param en_francais (Optional) If TRUE, the defaults for `base_title` and `layers_title` will be in French.
#'   Default is `FALSE.`
#' @inheritParams make_leaflet_map
#' @export
add_control_titles <- function(
  map,
  base_title = if (en_francais) "Th\u00E9mes" else "Basemaps",
  layers_title = if (en_francais) "Couches" else "Layers",
  en_francais = FALSE,
  as_reference = FALSE
) {
  stopifnot("leaflet" %in% class(map))
  stopifnot(
    is.character(base_title) | is.null(base_title),
    length(base_title) == 1 | is.null(base_title)
  )
  stopifnot(
    is.character(layers_title) | is.null(base_title),
    length(layers_title) == 1 | is.null(base_title)
  )

  # Handle NULLs
  if (is.null(base_title)) {
    base_title <- ""
  }
  if (is.null(layers_title)) {
    layers_title <- ""
  }

  # Build JS for inserting titles
  base_title <- base_title |> escape_symbol("'")
  layers_title <- layers_title |> escape_symbol("'")
  on_render_js <- "(el, x) => add_control_titles('%s', '%s')" |>
    sprintf(base_title, layers_title) |>
    gsub(pattern = "''", replacement = "null", fixed = TRUE)

  # Include js file inline in the header along with on_render_js inline
  js_path <- "js/add_control_titles.js" |>
    system.file(package = "aqmapr")
  map |>
    include_scripts(paths = js_path, as_reference = as_reference) |>
    htmlwidgets::onRender(on_render_js)
}
