#' Add multiple WMS layers to a map
#'
#' Adds multiple WMS layers to a Leaflet map using the WMS class.
#' Build WMS layers with [make_wms_layers()] or manually with [WMS()].
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
#' @param urls,layers,styles,opacities,legend_urls,legend_positions,formats,versions
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
  versions = "1.1.1"
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
      version = versions
    )

  inputs$group |>
    handyr::for_each(
      .enumerate = TRUE,
      .as_list = TRUE,
      .show_progress = FALSE,
      .name = TRUE,
      \(group, i) {
        WMS(
          url = inputs$url[i],
          legend_url = inputs$legend_url[i],
          legend_position = inputs$legend_position[i],
          layer = inputs$layer[i],
          group = group,
          format = inputs$format[i],
          style = inputs$style[i],
          opacity = inputs$opacity[i],
          version = inputs$version[i]
        )
      }
    )
}

#' WMS S7 class
#'
#' Represent a single Web Map Service (WMS) layer to be added to the map via add_wms_layers
#'
#' @export
#' @import S7
WMS <- new_class(
  "WMS",
  parent = LeafletLayer,
  properties = list(
    url = class_character |>
      new_property(validator = validator_len_1),
    layer = class_character |>
      new_property(validator = validator_len_1),
    style = class_character |>
      new_property(
        validator = validator_len_1,
        setter = \(self, value) {
          self@style <- is.null(value) |>
            ifelse(self@layer, value)
          return(self)
        }
      ),
    group = class_character |>
      new_property(
        setter = \(self, value) {
          self@group <- is.null(value) |>
            ifelse(self@layer, value)
          return(self)
        }
      ),
    legend_url = class_character |>
      new_property(
        setter = \(self, value) {
          if (length(value) == 0) {
            value <- self@url |>
              make_wms_legend_url(self@layer, self@style)
          } else if (is.na(value)) {
            value <- self@url |>
              make_wms_legend_url(self@layer, self@style)
          }
          self@legend_url <- value
          return(self)
        }
      ),
    format = class_character |>
      new_property(
        default = "image/png",
        validator = validator_len_1
      ),
    version = class_character |>
      new_property(
        default = "1.1.1",
        validator = validator_len_1
      ),
    crs = class_character
  )
)

make_wms_legend_url <- function(url, layer, style) {
  template <- "%s?service=WMS&request=GetLegendGraphic&format=image/png&transparent=true&layer=%s&style=%s"
  template |>
    sprintf(url, layer, style)
}

# Define method to add WMS layer to map
S7::method(add_to_map, WMS) <- function(layer, map) {
  # TODO: add subtitle using <small></small>
  legend_template <- "<strong>%s</strong><br/><img src = '%s'/>"
  on_render_template <- "function(el, x) {
    this.on('overlayadd', (e) => {if (e.name === '%s') toggleLegend('%s', true)});
    this.on('overlayremove', (e) => {if (e.name === '%s') toggleLegend('%s', false);});
  }"

  map |>
    leaflet::addWMSTiles(
      baseUrl = layer@url,
      layers = layer@layer,
      group = layer@group,
      options = list(
        format = layer@format,
        styles = layer@style,
        opacity = layer@opacity,
        transparent = layer@transparent
      )
    ) |>
    append_to_layer_control(layer_groups = layer@group) |>
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
          layer@group,
          layer@group,
          layer@group
        )
    )
}
