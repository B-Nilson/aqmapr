# Create leaflet map similiar to AQmap
make_leaflet_map <- function(marker_data = NULL) {
  map <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap)

  if (!is.null(marker_data)) {
    map <- map |>
      add_obs_markers(marker_data = marker_data) |>
    leaflet::addLayersControl(
      overlayGroups = levels(marker_data$network) |>
        pretty_text()
    )
  }

  return(map)
}

add_obs_markers <- function(map, marker_data) {
  icon_size <- list("obs" = 32, "missing_obs" = 16)

  # Add helper columns
  marker_data <- marker_data |>
    dplyr::mutate(
      # Determine pane to use based on pm25_1hr missing or not
      pane = is.na(pm25_1hr) |>
        ifelse(names(icon_size)[2], names(icon_size)[1]),
      # Select icon size similarily - smaller for missing obs
      icon_width = unname(unlist(icon_size[pane])),
      icon_height = icon_width,
      # Build url to icon
      icon_url = network |>
        make_aqmap_marker_icon_url(pm25_1hr = pm25_1hr)
    )

  # Add markers to map - 1 pane for missing, 1 for not
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
    )
}
