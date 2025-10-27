#' Leaflet polygon layer S7 class
#'
#' Represent a leaflet polygon layer to be added to the map using [add_to_map()].
#'
#' @export
#' @import S7
#' @include s7_classes.R
PolygonLayer <- new_class(
  "PolygonLayer",
  parent = LeafletLayer,
  properties = list(
    data = class_data.frame |>
      new_property(validator = \(value) {
        if (!is.null(value) & ncol(value) & nrow(value)) {
          if (!"sf" %in% class(value)) {
            "must be an `sf` data.frame"
          }
          if (
            !all(
              as.character(sf::st_geometry_type(value$geometry)) %in%
                c("POLYGON", "MULTIPOLYGON")
            )
          ) {
            "all geometries must be `POLYGON` or `MULTIPOLYGON`"
          }
        }
      }),
    data_url = class_character,
    data_url_columns = class_list |>
      new_property(
        default = list(
          iconUrl = "iconUrl",
          pane = "pane",
          zIndexOffset = "zIndexOffset",
          iconSize = "iconSize",
          label = "label"
        )
      ),
    use_stroke = class_flag_on,
    stroke_width = class_double |>
      new_property(default = 1),
    stroke_opacity = class_double |>
      new_property(default = 1),
    stroke_dash_array = class_character,
    smooth_factor = class_double |>
      new_property(default = 1),
    no_clip = class_flag_off,
    colour = class_colour |>
      new_property(default = "black", setter = colour_setter),
    color = color_property(),
    colour_palette = class_function |>
      new_property(default = \(x) rep("black", length(x))),
    colour_values = class_vector,
    use_fill = class_flag_on,
    fill = class_colour |>
      new_property(default = "grey", setter = fill_setter),
    fill_palette = class_function |>
      new_property(default = \(x) rep("grey", length(x))),
    fill_values = class_vector,
    popup = class_any,
    popup_options = class_list |>
      new_property(default = leaflet::popupOptions()),
    label = class_any,
    label_options = class_list |>
      new_property(default = leaflet::labelOptions()),
    highlight_options = class_list |>
      new_property(default = leaflet::highlightOptions()),
    options = class_list |>
      new_property(default = leaflet::pathOptions())
  )
)

# Define method to add WMS layer to map
S7::method(add_to_map, PolygonLayer) <- function(layer, map) {
  pane_name <- ifelse(is.list(layer@pane), layer@pane$name, layer@pane)
  if (!length(layer@data_url)) {
    map <- map |>
      leaflet::addPolygons(
        data = layer@data,
        layerId = layer@layer_id,
        group = layer@group,
        stroke = layer@use_stroke,
        color = layer@colour,
        weight = layer@stroke_width,
        opacity = layer@stroke_opacity,
        fill = layer@use_fill,
        fillColor = layer@fill,
        fillOpacity = layer@opacity,
        dashArray = layer@stroke_dash_array,
        smoothFactor = layer@smooth_factor,
        noClip = layer@no_clip,
        popup = layer@popup,
        popupOptions = layer@popup_options,
        label = layer@label,
        labelOptions = layer@label_options,
        highlightOptions = layer@highlight_options,
        options = c(list(pane = pane_name), layer@options)
      )
  }
  return(map)
}
