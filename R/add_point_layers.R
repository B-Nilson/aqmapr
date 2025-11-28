#' Leaflet point layer S7 class
#'
#' Represent a leaflet point layer to be added to the map using [add_to_map()].
#'
#' @export
#' @import S7
#' @include s7_classes.R
PointLayer <- new_class(
  "PointLayer",
  parent = LeafletLayer,
  properties = list(
    data = class_data.frame |>
      new_property(
        setter = \(self, value) {
          if (is.null(value)) {
            self@data <- value
          } else if (!"sf" %in% class(value) & ncol(value) & nrow(value)) {
            self@data <- value |>
              sf::st_as_sf(coords = c(self@x_col, self@y_col), crs = self@crs)
          } else {
            self@data <- value
          }
          return(self)
        },
        validator = \(value) {
          if (!is.null(value) & ncol(value) & nrow(value)) {
            if (!"sf" %in% class(value)) {
              "must be an `sf` data.frame"
            } else if (
              !all(
                as.character(sf::st_geometry_type(value$geometry)) %in%
                  c("POINT", "MULTIPOINT")
              )
            ) {
              "all geometries must be `POINT` or `MULTIPOINT`"
            }
          }
        }
      ),
    x_col = class_character |>
      new_union(class_integer) |>
      new_property(default = "lng", validator = validator_len_0_1),
    y_col = class_character |>
      new_union(class_integer) |>
      new_property(default = "lat", validator = validator_len_0_1),
    crs = class_character |>
      new_property(default = "WGS84", validator = validator_len_0_1),
    data_url = class_character,
    data_url_columns = class_list |>
      new_property(
        default = list(
          iconUrl = "iconUrl",
          pane = "pane",
          zIndexOffset = "zIndexOffset",
          iconSize = "iconSize",
          label = "label",
          popup = "popup"
        )
      ),
    icon_urls = class_character,
    use_stroke = class_flag_on,
    stroke_width = class_double |>
      new_property(default = 1),
    stroke_opacity = class_double |>
      new_property(default = 1),
    stroke_dash_array = class_character,
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
    radius = class_double |>
      new_property(default = 5),
    popup = class_any,
    popup_options = class_list |>
      new_property(default = leaflet::popupOptions()),
    label = class_any,
    label_options = class_list |>
      new_property(default = leaflet::labelOptions()),
    cluster_id = class_any,
    cluster_options = class_any, # if not NULL, then enables clusters...
    options = class_list |>
      new_property(default = leaflet::pathOptions())
  )
)

# Define method to add WMS layer to map
S7::method(add_to_map, PointLayer) <- function(layer, map) {
  pane_name <- ifelse(is.list(layer@pane), layer@pane$name, layer@pane)
  if (!length(layer@data_url)) {
    map <- map |>
      leaflet::addCircleMarkers(
        data = layer@data,
        radius = layer@radius,
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
        popup = layer@popup,
        popupOptions = layer@popup_options,
        label = layer@label,
        labelOptions = layer@label_options,
        clusterOptions = layer@cluster_options,
        clusterId = layer@cluster_id,
        options = c(list(pane = pane_name), layer@options)
      )
    if (length(layer@group)) {
      map <- map |>
        append_to_layer_control(layer_groups = layer@group)
    }
  }

  return(map)
}
