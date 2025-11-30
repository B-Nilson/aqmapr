#' Add multiple WMS layers to a map
#'
#' Adds multiple WMS layers to a Leaflet map using the WMS class.
#' Build WMS layers with [make_wms_layers()] or manually with [WMSLayer()].
#'
#' @param map A Leaflet map object
#' @param wms_layers A list of WMS class objects. See [make_wms_layers()] for details.
#' @export
#' @return A Leaflet map object with added WMS layers
add_wms_layers <- function(map, wms_layers) {
  for (wms_layer in wms_layers) {
    map <- map |> add_to_map(layer = wms_layer)
  }
  return(map)
}

#' Build WMS layer objects for adding to a map
#'
#' @param urls,layers,styles,opacities,legend_urls,legend_positions,formats,versions,display_by_defaults
#'   1 or more character (or numeric for `opacities`) values of the URL(s) of the WMS service,
#'   layer name(s) and style(s) to display at set opacities, legend URL(s) and position(s), layer format(s), and WMS version(s).
#'   Inputs will be recycled to a common length - single values will be repeated as needed.
#'   The names of `layers` will be used as the layer group names if present.
#' @return wms_layer object
#' @export
make_wms_layers <- function(
  urls,
  layers,
  styles,
  opacities = 0.8,
  legend_urls = NA,
  legend_positions = "bottomleft",
  formats = "image/png",
  versions = "1.1.1",
  display_by_defaults = FALSE
) {
  if (is.null(names(layers))) {
    names(layers) <- layers
  }
  # Combine and recyle inputs as needed
  inputs <- unname(layers) |>
    data.frame(
      layer = _,
      group = names(layers),
      url = urls,
      style = styles,
      opacity = opacities,
      legend_url = legend_urls,
      legend_position = legend_positions,
      format = formats,
      version = versions,
      display_by_default = display_by_defaults
    )

  inputs$group |>
    handyr::for_each(
      .enumerate = TRUE,
      .as_list = TRUE,
      .show_progress = FALSE,
      .name = TRUE,
      \(group, i) {
        WMSLayer(
          url = inputs$url[i],
          legend_url = inputs$legend_url[i],
          legend_position = inputs$legend_position[i],
          layer = inputs$layer[i],
          group = group,
          format = inputs$format[i],
          style = inputs$style[i],
          opacity = inputs$opacity[i],
          version = inputs$version[i],
          display_by_default = inputs$display_by_default[i]
        )
      }
    )
}

make_wms_legend_url <- function(url, layer, style) {
  template <- "%s?service=WMS&request=GetLegendGraphic&format=image/png&transparent=true&layer=%s&style=%s"
  template |>
    sprintf(url, layer, style)
}

# Define method to add WMS layer to map
S7::method(add_to_map, WMSLayer) <- function(layer, map) {
  # TODO: add subtitle using <small></small>
  legend_template <- "<strong>%s</strong><br/><img src = '%s'/>"
  on_render_template <- "function(el, x) {
    toggleLegend('%s', %s);
    this.on('overlayadd', (e) => {if (e.name === '%s') toggleLegend('%s', true)});
    this.on('overlayremove', (e) => {if (e.name === '%s') toggleLegend('%s', false);});
  }"

  pane_name <- is.list(layer@pane) |>
    ifelse(layer@pane$name, layer@pane)

  map |>
    leaflet::addWMSTiles(
      baseUrl = layer@url,
      layers = layer@layer,
      group = layer@group,
      options = list(
        format = layer@format,
        styles = layer@style,
        opacity = layer@opacity,
        transparent = layer@transparent,
        pane = pane_name
      )
    ) |>
    # Add legend
    leaflet::addControl(
      layerId = layer@group |>
        stringr::str_replace_all(" |\\.", "_"),
      html = legend_template |>
        sprintf(layer@group, layer@legend_url),
      position = layer@legend_position
    ) |>
    # Link legend to layer control
    include_scripts(
      paths = system.file("js/toggle_legend.js", package = "aqmapr"),
      as_reference = FALSE
    ) |>
    htmlwidgets::onRender(
      on_render_template |>
        sprintf(
          layer@group,
          tolower(layer@display_by_default),
          layer@group,
          layer@group,
          layer@group,
          layer@group
        )
    )
}
