# Create leaflet map similiar to AQmap
make_aqmap <- function(
  networks = c("agency", "lcm"),
  base_maps = c(
    "Light Theme" = "OpenStreetMap",
    "Dark Theme" = "CartoDB.DarkMatter"
  ),
  js_dir = system.file("js", package = "aqmapr"),
  js_endpoint = "/js",
  css_dir = system.file("css", package = "aqmapr"),
  css_endpoint = "/css",
  use_references = TRUE,
  page_title = "AQmap"
) {
  # javascript/css files to include
  js_files <- c(
    "aqhi.js",
    "make_monitor_popup.js",
    "make_monitor_tooltip.js",
    "map_layers.js",
    "on_render.js"
  )
  if (!use_references) {
    js_paths <- file.path(js_dir, js_files)
  } else {
    js_paths <- file.path(js_endpoint, js_files)
  }
  css_files <- "monitor_popup.css"
  if (!use_references) {
    css_paths <- file.path(css_dir, css_files)
  } else {
    css_paths <- file.path(css_endpoint, css_files)
  }

  # Define layers to display
  point_layers <- networks |> make_aqmap_point_layers()
  polygon_layers <- make_aqmap_polygon_layers()
  wms_layers <- make_aqmap_wms_layers()

  # Build basemap
  map <- base_maps |>
    make_leaflet_map(
      track_map_state = TRUE,
      center_on_opened_popup = TRUE,
      as_reference = use_references,
      include_timestamp = TRUE,
      point_layers = point_layers,
      polygon_layers = polygon_layers,
      wms_layers = wms_layers,
      page_title = page_title
    ) |>
    # Include custom css/js used by various parts of the map
    include_scripts(paths = c(js_paths, css_paths), as_reference = use_references) |>
    htmlwidgets::onRender("handle_page_render")

  # Add offline/online panes and icon legend
  if (length(networks)) {
    map <- map |>
      leaflet::addMapPane("offline", zIndex = 415) |>
      leaflet::addMapPane("online", zIndex = 420) |>
      add_monitor_legend(
        networks = networks,
        position = "bottomright"
      )
  }

  return(map)
}

make_aqmap_point_layers <- function(networks) {
  placeholders_arg <- paste(
    "{ date_stamp: data.%s,",
    "station_name: data.%s,",
    "monitor_type: data.%s,",
    "health_message: aqhi_health_messages[get_aqhi_category(data.%s)] }"
  ) |>
    sprintf("date_stamp", "name", "network_type", "pm25_1hr")

  values_arg <- paste(
    "{ pm25_10min: data.%s,",
    "pm25_1hr: data.%s,",
    "pm25_3hr: data.%s,",
    "pm25_24hr: data.%s }"
  ) |>
    sprintf("pm25_10min", "pm25_1hr", "pm25_3hr", "pm25_24hr")

  # Build popup/tooltip
  popup <- "JS:::make_monitor_popup(%s, %s);" |>
    sprintf(placeholders_arg, values_arg)
  tooltip <- "JS:::make_monitor_tooltip(%s, %s);" |>
    sprintf(placeholders_arg, values_arg) # TODO: ensure tooltip args match...

  data_url_columns <- list(
    iconUrl = "iconUrl",
    pane = "pane",
    zIndexOffset = "zIndexOffset",
    iconSize = "iconSize"
  )
  networks |>
    lapply(\(network) {
      # Remove 10min placeholder from popup/tooltip templates for agency monitors
      url_columns <- data_url_columns
      url_columns$popup <- (network == "agency") |>
        ifelse(popup |> stringr::str_remove("pm25_10min: .+?, "), popup)
      url_columns$label <- (network == "agency") |>
        ifelse(tooltip |> stringr::str_remove("pm25_10min: .+?, "), tooltip)
      # Build layer
      PointLayer(
        group = pretty_text(network),
        data_url = "/data/recent/%s/geojson" |> sprintf(network),
        data_url_columns = url_columns,
        display_by_default = TRUE
      )
    })
}

make_aqmap_polygon_layers <- function() {
  rlang::check_installed("canadata")
  eer_smoke <- get_eccc_eer_smoke()
  hms_smoke <- get_noaa_hms_smoke()

  polygon_layers <- list(
    PolygonLayer(
      data = canadata::provinces_and_territories,
      opacity = 0.2
    ),
    PolygonLayer(
      group = "Modelled Smoke",
      data = eer_smoke,
      fill = ~min_pm25,
      fill_palette = eer_smoke_pal(),
      display_by_default = FALSE
    ) |>
      handyr::on_error(.return = NULL, .warn = TRUE),
    PolygonLayer(
      group = "Visible Smoke",
      data = hms_smoke,
      fill = ~density,
      fill_palette = hms_smoke_pal(),
      display_by_default = FALSE
    ) |>
      handyr::on_error(.return = NULL, .warn = TRUE)
  )
  missing_layers <- sapply(polygon_layers, is.null)
  if (!all(missing_layers)) {
    return(polygon_layers[which(!missing_layers)])
  } else {
    return(list())
  }
}

make_aqmap_wms_layers <- function() {
  make_wms_layers(
    urls = "https://geo.weather.gc.ca/geomet",
    layers = c(
      "Surface Winds" = "HRDPS.CONTINENTAL_UU",
      "Modelled PM2.5" = "RAQDPS.SFC_PM2.5"
    ),
    styles = c("WindBarbs_Sfc", "RAQDPS-SFC-PM_UGM3_BCAQHI"),
    # TODO: remove \once v3.6 is released
    legend_urls = "https://aqmap.ca/aqmap/dev/icons/windbarbs_legend.jpg" |>
      c(NA),
    legend_positions = "bottomleft",
    formats = "image/png",
    opacities = 0.6
  ) |>
    c(make_wms_layers(
      urls = "https://cwfis.cfs.nrcan.gc.ca/geoserver/ows",
      layers = c(
        "Active Fires" = "public:activefires_current",
        "Fire Perimeters" = "m3_polygons_current",
        "Fire Danger" = "public:fdr_current"
      ),
      styles = c(
        "public:cwfis_activefires",
        "cwfis_m3_polygons",
        "public:cffdrs_fdr"
      ),
      formats = "image/png",
      legend_positions = "bottomright",
      opacities = 0.6
    ))
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
    "name",
    "network_type",
    "date_stamp" = "date",
    "pm25_10min",
    "pm25_1hr",
    "pm25_3hr",
    "pm25_24hr"
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
      iconSize = ifelse(is.na(.data$pm25_1hr), 21, 30),
      network_type = .data$monitor_type |>
        factor(
          levels = c("FEM", "PA", "EGG"),
          labels = c("Regulatory (FEM)", "PurpleAir (PA)", "AQegg (EGG)")
        ),
    ) |>
    dplyr::select(dplyr::all_of(desired_cols)) |>
    sf::st_as_sf(coords = c("lng", "lat"))
}
