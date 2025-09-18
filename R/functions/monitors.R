add_obs_markers <- function(map, marker_data) {
  # Ensure icons exist
  marker_data$network |>
    make_icon_svg(
      pm25_1hr = marker_data$pm25_1hr,
      force = .force_update_icons
    )

  # Add helper columns
  marker_data <- marker_data |>
    dplyr::mutate(
      # Determine pane to use based on pm25_1hr missing or not
      pane = is.na(pm25_1hr) |>
        ifelse(names(.marker_sizes)[1], names(.marker_sizes)[2]),
      # Select icon size similarily - smaller for missing obs
      icon_width = unname(unlist(.marker_sizes[pane])),
      icon_height = icon_width,
      # Build url to icon
      icon_url = network |>
        make_marker_icon_path(pm25_1hr = pm25_1hr)
    )

  # Add markers to map - 1 pane for missing, 1 for not
  map <- map |>
    leaflet::addMapPane(names(.marker_sizes)[1], zIndex = 650) |>
    leaflet::addMapPane(names(.marker_sizes)[2], zIndex = 660) |>
    leaflet::addMarkers(
      data = marker_data,
      group = ~ as.character(network) |>
        pretty_text(),
      options = ~ leaflet::pathOptions(pane = pane) |>
        c(leaflet::markerOptions(zIndexOffset = round(pm25_1hr * 10))),
      lng = ~lng,
      lat = ~lat,
      icon = ~ leaflet::icons(
        iconUrl = icon_url,
        iconWidth = icon_width,
        iconHeight = icon_height
      )
    )
}
