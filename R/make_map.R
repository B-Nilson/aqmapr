# Create leaflet map similiar to AQmap
make_aqmap <- function(
  marker_data = NULL,
  base_maps = c("Light Theme" = "OpenStreetMap"),
  template_dir,
  icon_dirs,
  font_sizes,
  marker_sizes,
  pm25_units,
  text,
  force_update_icons = FALSE
) {
  map <- leaflet::leaflet() |>
    add_base_maps(base_maps) |>
    # Use leaflet.extras::addHash() + custom js
    # to track map location/layers/basemap
    track_map_state()

  if (!is.null(marker_data)) {
    map <- map |>
      add_obs_markers(
        marker_data = marker_data,
        template_dir = template_dir,
        icon_dir = icon_dirs$local,
        font_sizes = font_sizes$markers,
        marker_sizes = marker_sizes,
        pm25_units = pm25_units,
        marker_hover_text = text$monitor_hover,
        force_update_icons = force_update_icons
      ) |>
      add_monitor_legend(
        networks = levels(marker_data$network),
        legend_details = text$monitor_legend,
        icon_dir = icon_dirs$server,
        marker_size = marker_sizes$legend
      ) |>
      leaflet::addLayersControl(
        baseGroups = names(base_maps),
        overlayGroups = levels(marker_data$network) |>
          pretty_text(),
      )
  }

  return(map)
}
