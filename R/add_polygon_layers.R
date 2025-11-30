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
