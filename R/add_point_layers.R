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
