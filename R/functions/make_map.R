# Create leaflet map similiar to AQmap
make_leaflet_map <- function(marker_data = NULL) {
  map <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap)

  if (!is.null(marker_data)) {
    # TODO: add default icon_url if value/column is missing
    map <- map |>
      leaflet::addMarkers(
        data = marker_data,
        group = ~as.character(network) |> 
          pretty_text(), 
        lng = ~lng,
        lat = ~lat,
        icon = ~ leaflet::icons(icon_url, iconWidth = 32, iconHeight = 32)
      ) |> 
      leaflet::addLayersControl(
        overlayGroups = levels(marker_data$network) |> 
          pretty_text(),
      )
  }

  return(map)
}
