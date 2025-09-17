# Create leaflet map similiar to AQmap
make_leaflet_map <- function(marker_data = NULL) {
  map <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap)

  if (!is.null(marker_data)) {
    icon_size <- list("obs" = 32, "missing_obs" = 16)
    marker_data <- marker_data |>
      dplyr::mutate(
        pane = is.na(pm25_1hr) |>
          ifelse(names(icon_size)[2], names(icon_size)[1]),
        icon_width = unname(unlist(icon_size[pane])),
        icon_height = icon_width
      )
    # TODO: add default icon_url if value/column is missing
    map <- map |>
      leaflet::addMapPane(names(icon_size)[2], zIndex = 650) |>
      leaflet::addMapPane(names(icon_size)[1], zIndex = 660) |>
      leaflet::addMarkers(
        data = marker_data,
        group = ~ as.character(network) |>
          pretty_text(),
        options = ~ leaflet::pathOptions(pane = pane),
        lng = ~lng,
        lat = ~lat,
        icon = ~ leaflet::icons(
          icon_url,
          iconWidth = icon_width,
          iconHeight = icon_height
        )
      ) |>
      leaflet::addLayersControl(
        overlayGroups = levels(marker_data$network) |>
          pretty_text(),
      )
  }

  return(map)
}
