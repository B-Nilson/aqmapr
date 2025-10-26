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
        if (!"sf" %in% class(value) & ncol(value) & nrow(value)) {
          "must be an `sf` data.frame"
        }
        if (! all(as.character(sf::st_geometry_type(value$geometry)) %in% c("POLYGON", "MULTIPOLYGON"))) {
          "all geometries must be `POLYGON` or `MULTIPOLYGON`"
        }
      }),
    data_url = class_character,
    use_stroke = class_logical |>
      new_property(
        default = TRUE,
        validator = validator_len_1
      ),
    stroke_width = class_double |>
      new_property(
        default = 1,
      ),
    stroke_opacity = class_double |>
      new_property(
        default = 1,
      ),
    stroke_dash_array = class_character,
    smooth_factor = class_double |>
      new_property(default = 1),
    no_clip = class_logical |>
      new_property(default = FALSE),
    colour = class_colour |>
      new_property(
        default = "black",
        setter = \(self, value) {
          if (
            is.list(value) &
              identical(sort(names(value)), c("palette", "values"))
          ) {
            self@colour_palette <- value$palette
            if ("formula" %in% class(value$palette) & !is.null(self@data)) {
              self@colour_values <- self@data |>
                dplyr::pull(!!rlang::as_quosure(value$palette))
            }
            self@colour_values <- value$values
          } else if ("formula" %in% class(value) & !is.null(self@data)) {
            self@colour_values <- self@data |>
              dplyr::pull(!!rlang::as_quosure(value))
            value <- self@colour_palette(self@colour_values)
          }
          self@colour <- value
          return(self)
        }
      ),
    color = new_property(
      class_colour,
      default = quote(colour),
      getter = function(self) {
        self@colour
      },
      setter = function(self, value) {
        if (identical(value, self@colour)) {
          return(self)
        }
        self@colour <- value
        self
      }
    ),
    colour_palette = class_function |>
      new_property(default = \(x) rep("black", length(x))),
    colour_values = class_vector,
    use_fill = class_logical |>
      new_property(
        default = TRUE,
        validator = validator_len_1
      ),
    fill = class_colour |>
      new_property(
        default = "grey",
        setter = \(self, value) {
          if (
            is.list(value) &
              identical(sort(names(value)), c("palette", "values"))
          ) {
            self@fill_palette <- value$palette
            self@fill_values <- value$values
          } else if ("formula" %in% class(value) & !is.null(self@data)) {
            self@fill_values <- self@data |>
              dplyr::pull(!!rlang::as_quosure(value))
            value <- self@fill_palette(self@fill_values)
          }
          self@fill <- value
          return(self)
        }
      ),
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
      new_union(class_any) |>
      new_property(setter = \(self, value) {
        if (length(value) == 0) {
          self@highlight_options <- NULL
        }else {
          self@highlight_options <- value
        }
        return(self)
      }
      ),
    options = class_list |>
      new_property(default = leaflet::pathOptions())
  )
)

# Define method to add WMS layer to map
S7::method(add_to_map, PolygonLayer) <- function(layer, map) {
  # Create custom panes as needed
  if (is.list(layer@pane)) {
    map <- map |>
      leaflet::addMapPane(
        name = layer@pane$name,
        zIndex = layer@pane$zindex
      )
    pane_name <- layer@pane$name
  } else {
    pane_name <- layer@pane
  }

  if (length(layer@data_url)) {
    map <- map |>
      add_geojson_layer(
        json_url = layer@data_url,
        group = layer@group,
        add_to_layer_control = length(layer@group) > 0,
        as_reference = TRUE
      )
  } else {
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
    if (length(layer@group)) {
      map <- map |>
        append_to_layer_control(
          layer_groups = layer@group
        )
    }
  }
  if (layer@use_fill & length(layer@fill_values)) {
    map <- map |>
      leaflet::addLegend(
        data = layer@data,
        group = layer@group,
        pal = layer@fill_palette,
        values = layer@fill_values,
        opacity = layer@opacity,
        position = layer@legend_position,
        title = layer@group
      )
  }
  return(map)
}
