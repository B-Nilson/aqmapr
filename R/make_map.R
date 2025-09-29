# Create leaflet map similiar to AQmap
make_aqmap <- function(
  marker_data = NULL,
  base_maps = c("Light Theme" = "OpenStreetMap"),
  font_sizes,
  marker_sizes,
  pm25_units,
  text,
  template_dir = system.file("images", package = "aqmapr"),
  icon_dir = system.file("images/icons", package = "aqmapr"),
  icon_endpoint = "/icons",
  js_dir = system.file("js", package = "aqmapr"),
  js_endpoint = "/js",
  css_dir = system.file("css", package = "aqmapr"),
  css_endpoint = "/css",
  force_update_icons = FALSE
) {
  # General javascript files
  js_files <- c("map_layers.js", "on_render.js")
  js_paths <- file.path(js_endpoint, js_files)

  # Build basemap
  map <- leaflet::leaflet() |>
    add_base_maps(base_maps = base_maps) |>
    # Include custom js used by various parts of the map
    include_scripts(paths = js_paths, types = "js") |>
    htmlwidgets::onRender("handle_page_render") |>
    # Use leaflet.extras::addHash() + custom js
    # to track map location/layers/basemap
    track_map_state(
      js_dir = js_dir,
      js_endpoint = js_endpoint
    ) |>
    # Cache provider tiles for faster reload times
    leaflet.extras::enableTileCaching()

  # Add observation markers
  if (!is.null(marker_data)) {
    map <- map |>
      add_obs_markers(
        marker_data = marker_data,
        template_dir = template_dir,
        icon_dir = icon_dir,
        font_sizes = font_sizes$markers,
        marker_sizes = marker_sizes,
        pm25_units = pm25_units,
        marker_hover_text = text$monitor_hover,
        force_update_icons = force_update_icons
      ) |>
      add_monitor_legend(
        networks = levels(marker_data$network),
        legend_title = text$monitor_legend$title |>
          stats::setNames(text$monitor_legend$hover),
        icon_dir = icon_endpoint,
        css_dir = css_dir,
        css_endpoint = css_endpoint,
        position = "bottomright"
      ) |>
      append_to_layer_control(
        layer_groups = levels(marker_data$network) |>
          pretty_text()
      )
  }

  return(map)
}
