#' Leaflet generic layer S7 class
#'
#' Represent a leaflet layer to be added to the map. Usually expanded by other classes then added to a map using [add_to_map()].
#' 
#' @param group,layer_id,class_name (Optional). 
#'   A character string of the layer group/id/class to attach to the layer.
#'   Default is NULL (no group/id/class).
#' @param pane (Optional). A character string of the map pane to place the layer.
#' @param legend_position (Optional).
#'   A character string of the legend position to attach to the layer.
#'   Default is "bottomleft".
#' @param display_by_default (Optional).
#'   A logical value indicating whether the layer should be visible on the map by default.
#'   Default is TRUE.
#' @param opacity (Optional).
#'   A numeric value between 0 and 1 indicating the opacity of the layer.
#'   Default is 0.8.
#' @param transparent (Internal - not to be used).
#'   A logical value indicating whether the layer should be transparent.
#'   Is set to TRUE if opacity < 1.
#' @param interactive (Optional).
#'   A logical value indicating whether the layer should be interactive or static.
#'   Default is TRUE (interactive layer).
#' @param bubbling_mouse_events (Optional).
#'   A logical value indicating whether the layer should bubble mouse events.
#'   Default is TRUE.
#'   
#' @export
#' @import S7
LeafletLayer <- new_class(
  "LeafletLayer",
  properties = list(
    group = class_character |>
      new_property(validator = validator_len_0_1),
    layer_id = class_character,
    class_name = class_character,
    pane = class_leaflet_pane,
    legend_position = class_leaflet_position,
    display_by_default = class_flag_on,
    opacity = class_double |>
      new_property(
        default = 0.8,
        validator = validator_len_1
      ),
    transparent = class_logical |>
      new_property(
        default = FALSE,
        validator = validator_len_1,
        getter = \(self) {
          self@opacity < 1
        }
      ),
    interactive = class_flag_on,
    bubbling_mouse_events = class_flag_on
  )
)

#' Leaflet point layer S7 class
#'
#' Represent a leaflet point layer to be added to the map using [add_to_map()].
#' 
#' @inheritParams LeafletLayer
#' @param data (Optional). 
#'   A data.frame/sf of coordinates to create points from.
#' @param x_col,y_col (Optional).
#'   The name of the column containing the x/y coordinates if `data` is a non-sf data.frame.
#' @param crs (Optional).
#'   The coordinate reference system to use if `data` is a non-sf data.frame.
#' @param data_url (Optional).
#'   The URL to fetch data from for creating the layer.
#' @param data_url_columns (Optional).
#'   The columns to use from the data fetched from `data_url`.
#' @param icon_urls (Optional).
#'   A character vector of URLs to use for the icons. (IN DEVELOPMENT)
#' @param use_stroke (Optional).
#'   A logical value indicating whether to use a stroke for edge of the points.
#'   Defaults to `TRUE`.
#' @param stroke_width,stroke_opacity (Optional).
#'   A numeric value indicating the width and opacity of the stroke for the points.
#'   Defaults to `1`.
#' @param stroke_dash_array (Optional).
#'   A character value indicating the dash array for the stroke.
#' @param colour,fill (Optional).
#'   A character value or formula reference indicating the colour of the stroke or fill colour of the points.
#' @param color Alias for `colour`.
#' @param colour_palette,fill_palette (Optional).
#'   A function that returns inputs for `colour`/`fill` based on the `colour_values`/`fill_values`.
#' @param colour_values,fill_values (Optional).
#'   Values or a formula reference to be passed to `colour_palette`/`fill_palette`.
#' @param popup,label (Optional).
#'   Character value(s) or a formula reference to be used for the popup/label of the points.
#' @param popup_options,label_options (Optional).
#'   A list of options for the popup/label.
#'   See [leaflet::popupOptions()] and [leaflet::labelOptions()] for more details.
#' @param cluster_id (Optional).
#'   A character value to use for the cluster id.
#' @param cluster_options (Optional).
#'   A list of options for clustering the points.
#'   See [leaflet::markerClusterOptions()] for more details.
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

#' Leaflet polygon layer S7 class
#'
#' Represent a leaflet polygon layer to be added to the map using [add_to_map()].
#' 
#' @inheritParams LeafletLayer
#' @inheritParams PointLayer
#' @param no_clip (Optional). A logical value indicating whether to clip the layer to the map bounds. Defaults to `FALSE`.
#' @param smooth_factor (Optional). A numeric value indicating the smoothness of the polygon edges. Defaults to `1`.
#' @param highlight_options (Optional). A list of options for highlighting the polygons. See [leaflet::highlightOptions()] for more details.
#' @param options (Optional). A list of additional options to pass to the L.polygon() method. See [leaflet::pathOptions()] for more details.
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
          label = "label",
          popup = "popup"
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

#' WMS S7 class
#'
#' Represent a single Web Map Service (WMS) layer to be added to the map via [add_wms_layers()]
#'
#' @inheritParams LeafletLayer
#' @param url character. Base URL of the WMS service.
#' @param layer,style character. Name of the layer/style to display from the WMS service.
#' @param legend_url character. URL of the legend to display when the layer is visible.
#' @param legend_position character. Position of the legend to display for the layer. Must be one of "bottomleft", "bottomright", "topleft", "topright".
#' @param format character. Format of the layer (e.g. image/png).
#' @param version character. Version of the WMS service. Defaults to "1.1.1".
#' @param crs character. Coordinate reference system to use for the layer.
#' 
#' @export
#' @import S7
WMSLayer <- new_class(
  "WMSLayer",
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