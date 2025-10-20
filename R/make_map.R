# Create leaflet map similiar to AQmap
make_aqmap <- function(
  networks = c("agency", "lcm"),
  base_maps = c(
    "Light Theme" = "OpenStreetMap",
    "Dark Theme" = "CartoDB.DarkMatter"
  ),
  js_dir = system.file("js", package = "aqmapr"),
  js_endpoint = "/js",
  use_references = TRUE
) {
  # General javascript files
  js_files <- c("map_layers.js", "on_render.js")
  js_paths <- file.path(js_endpoint, js_files)

  # Get extra layers
  polygon_layers <- list(
    "Modelled Smoke" = get_eccc_eer_smoke_forecasts() |>
      handyr::on_error(.return = NULL),
    "Visible Smoke" = get_noaa_hms_smoke_polygons() |>
      handyr::on_error(.return = NULL)
  )
  polygon_options <- list(
    "Modelled Smoke" = list(
      fillColor = ~min_pm25,
      weight = 1,
      color = "black",
      fillOpacity = 0.8,
      opacity = 1,
      palette = eer_smoke_pal()
    ),
    "Visible Smoke" = list(
      fillColor = ~density,
      weight = 1,
      color = "black",
      fillOpacity = 0.8,
      opacity = 1,
      palette = hms_smoke_pal()
    )
  )

  # Build basemap
  map <- base_maps |>
    make_leaflet_map(
      track_map_state = TRUE,
      as_reference = use_references,
      include_timestamp = TRUE,
      polygon_data = polygon_layers,
      polygon_options = polygon_options
    ) |>
    # Include custom js used by various parts of the map
    include_scripts(paths = js_paths, as_reference = use_references) |>
    htmlwidgets::onRender("handle_page_render")

  # Add observation markers
  if (length(networks)) {
    map <- map |>
      add_obs_markers(
        networks = networks,
        as_reference = use_references
      )
  }

  return(map)
}

format_for_geojson <- function(out_data) {
  rlang::check_installed("sf")
  desired_cols <- c(
    "id",
    "lng",
    "lat",
    "pane",
    "zIndexOffset",
    "iconUrl",
    "iconSize",
    "label"
  )

  # Define visible text for hovers
  marker_hover_text <- list(
    type = "Type: ",
    time = "Time: ",
    monitor = "Monitor: ",
    pm_title = "PM<sub>2.5</sub> averages:",
    pm_10min = "10 min.:",
    pm_1hr = "1 hr.:",
    pm_3hr = "3 hr.:",
    pm_24hr = "24 hr.:",
    no_data = "No Data."
  )

  # Ensure icons exist
  out_data$network |>
    as.character() |>
    make_icon_svg(
      values = out_data$pm25_1hr,
      icon_dir = system.file("images/icons", package = "aqmapr"),
      marker_size_missing = 17,
      for_legend = FALSE,
      force = FALSE
    )

  # Reformat to geojson for populating map markers
  out_data |>
    dplyr::rename(id = "site_id", date = "date_last_obs") |>
    dplyr::mutate(
      pane = ifelse(is.na(.data$pm25_1hr), "offline", "online"),
      zIndexOffset = ifelse(
        is.na(.data$pm25_1hr),
        0,
        round(.data$pm25_1hr * 10)
      ),
      iconUrl = .data$network |>
        as.character() |>
        make_icon_path(
          values = .data$pm25_1hr,
          icon_dir = "/icons",
          for_legend = FALSE
        ),
      iconSize = ifelse(is.na(.data$pm25_1hr), 18, 32),
      label = make_monitor_hover(
        name = .data$name,
        network = .data$network,
        monitor_type = .data$monitor_type,
        date_last_obs = .data$date,
        pm25_10min = .data$pm25_10min,
        pm25_1hr = .data$pm25_1hr,
        pm25_3hr = .data$pm25_3hr,
        pm25_24hr = .data$pm25_24hr,
        text = marker_hover_text
      )
    ) |>
    dplyr::select(dplyr::all_of(desired_cols)) |>
    sf::st_as_sf(coords = c("lng", "lat"))
}
