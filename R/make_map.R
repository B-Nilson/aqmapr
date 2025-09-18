# Create leaflet map similiar to AQmap
make_leaflet_map <- function(
  marker_data = NULL,
  template_dir,
  icon_dirs,
  font_sizes,
  marker_sizes,
  text,
  force_update_icons = FALSE
) {
  map <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap)

  if (!is.null(marker_data)) {
    map <- map |>
      add_obs_markers(
        marker_data = marker_data,
        template_dir = template_dir,
        icon_dir = icon_dirs$local,
        font_sizes = font_sizes$markers,
        marker_sizes = marker_sizes,
        force_update_icons = force_update_icons
      ) |>
      add_monitor_legend(
        networks = levels(marker_data$network),
        legend_details = text$monitor_legend,
        icon_dir = icon_dirs$server,
        marker_size = marker_sizes$legend
      ) |>
      leaflet::addLayersControl(
        overlayGroups = levels(marker_data$network) |>
          pretty_text()
      )
  }

  return(map)
}
