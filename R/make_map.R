# Create leaflet map similiar to AQmap
make_aqmap <- function(
  marker_data = NULL,
  base_maps = c(
    "Light Theme" = "OpenStreetMap",
    "Dark Theme" = "CartoDB.DarkMatter"
  ),
  font_sizes = c(119, 99, 90),
  marker_sizes = list(missing = 17, obs = 33),
  monitor_hover_text = list(
        type = "Type: ",
        time = "Time: ",
        pm_title = "PM<sub>2.5</sub> averages:",
        pm_10min = "10 min.:",
        pm_1hr = "1 hr.:",
        pm_3hr = "3 hr.:",
        pm_24hr = "24 hr.:",
        no_data = "No Data."
      ),
  monitor_legend_title = c(
    "Fine particulate matter monitor observations" = "PM<sub>2.5</sub> Monitors"
  ),
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
        font_sizes = font_sizes,
        marker_sizes = marker_sizes,
        marker_hover_text = monitor_hover_text,
        force_update_icons = force_update_icons
      ) |>
      add_monitor_legend(
        networks = levels(marker_data$network),
        legend_title = monitor_legend_title,
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
