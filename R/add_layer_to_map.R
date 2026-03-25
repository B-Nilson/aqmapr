#' S7 generic to add a layer to a Leaflet map
#'
#' Add a layer to a map using [add_to_map()].
#'
#' @param layer A leaflet layer to add to the map. Created using [PointLayer()], [PolygonLayer()], or [WMSLayer()].
#' @param map A leaflet map to add the layer to.
#' @param ... Additional arguments to pass to the layer's `add_to_map` method.
#' @return A leaflet map with the layer added
#' @export
add_to_map <- "add_to_map" |>
  S7::new_generic(dispatch_args = "layer", fun = function(layer, map, ...) {
    # Create custom panes as needed
    if (is.list(layer@pane)) {
      map <- map |>
        leaflet::addMapPane(
          name = layer@pane$name,
          zIndex = layer@pane$zindex
        )
    }

    if (!"aqmapr::WMSLayer" %in% class(layer)) {
      pane_name <- is.list(layer@pane) |>
        ifelse(layer@pane$name, layer@pane)
      # Add referenced geojson if url provided
      if (length(layer@data_url)) {
        map <- map |>
          add_geojson_layer(
            layer_id = layer@layer_id,
            group = layer@group,
            json_url = layer@data_url,
            options = c(
              list(pane = pane_name),
              layer@options
            ),
            option_columns = layer@data_url_columns,
            add_to_layer_control = FALSE,
            display_on_load = layer@display_by_default,
            as_reference = TRUE
          )
      }
      # Add legend if desired
      legend_desired <- layer@use_fill &
        length(layer@fill_values) &
        length(layer@group) &
        !identical(layer@fill_values, layer@fill)
      if (legend_desired) {
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
    }

    # Add add to layer control if desired
    if (length(layer@group)) {
      map <- map |>
        append_to_layer_control(
          layer_groups = layer@group
        )
    }

    # Hide layer if desired
    if (!layer@display_by_default) {
      map <- map |>
        leaflet::hideGroup(group = layer@group)
    }

    S7::S7_dispatch()
  })

# Define method to add point layers to map
#' @include s7_layers.R
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

# Define method to add polygon layers to map
#' @include s7_layers.R
S7::method(add_to_map, PolygonLayer) <- function(layer, map) {
  pane_name <- if (is.list(layer@pane)) layer@pane$name else layer@pane
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

# Define method to add WMS layer to map
#' @include s7_layers.R
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
          layer@group |> escape_symbol("'"),
          tolower(layer@display_by_default),
          layer@group |> escape_symbol("'"),
          layer@group |> escape_symbol("'"),
          layer@group |> escape_symbol("'"),
          layer@group |> escape_symbol("'")
        )
    )
}
