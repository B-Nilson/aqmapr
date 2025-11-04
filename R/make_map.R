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

  # Define WMS layers to display
  wms_layers <- make_aqmap_wms_layers()

  # Define point layers to display
  popup_fn_template <- paste(
    "JS:::make_monitor_popup(",
    "data.%s,",
    "data.%s, {",
    "date_stamp: data.%s,",
    "pm25_10min: data.%s,",
    "pm25_1hr: data.%s,",
    "pm25_3hr: data.%s,",
    "pm25_24hr: data.%s});"
  )

  point_layers <- networks |>
    lapply(\(network) {
      # Build popup/tooltip
      popup <- popup_fn_template |>
        sprintf(
          "name",
          "network_type",
          "date_stamp",
          "pm25_10min",
          "pm25_1hr",
          "pm25_3hr",
          "pm25_24hr"
        )
      if (network == "agency") {
        popup <- popup |>
          stringr::str_remove("pm25_10min: .+?, ")
      }
      tooltip <- popup |>
        stringr::str_replace(
          "JS:::make_monitor_popup",
          "JS:::make_monitor_tooltip"
        )
      # Build layer
      PointLayer(
        group = pretty_text(network),
        data_url = "/data/recent/%s/geojson" |> sprintf(network),
        data_url_columns = list(
          iconUrl = "iconUrl",
          pane = "pane",
          zIndexOffset = "zIndexOffset",
          iconSize = "iconSize",
          label = tooltip,
          popup = popup
        ),
        display_by_default = TRUE
      )
    })

  # Get extra layers
  polygon_layers <- list(
    # get_eccc_eer_smoke_forecasts() |>
    #   PolygonLayer(
    #     group = "Modelled Smoke",
    #     data = _,
    #     fill = ~min_pm25,
    #     fill_palette = eer_smoke_pal(),
    #     display_by_default = FALSE
    #   ) |>
    stop("disabled for testing") |>
      handyr::on_error(.return = NULL, .warn = TRUE),
    # get_noaa_hms_smoke_polygons() |>
    # PolygonLayer(
    #   group = "Visible Smoke",
    #   data = _,
    #   fill = ~density,
    #   fill_palette = hms_smoke_pal(),
    #   display_by_default = FALSE
    # ) |>
    stop("disabled for testing") |>
      handyr::on_error(.return = NULL, .warn = TRUE)
  )
  polygon_layers <- polygon_layers[which(!sapply(polygon_layers, is.null))]

  # Build basemap
  map <- base_maps |>
    make_leaflet_map(
      track_map_state = TRUE,
      as_reference = use_references,
      include_timestamp = TRUE,
      point_layers = point_layers,
      polygon_layers = polygon_layers,
      wms_layers = wms_layers
    ) |>
    # Include custom js used by various parts of the map
    include_scripts(paths = js_paths, as_reference = use_references) |>
    include_scripts(
      paths = system.file("css/monitor_popup.css", package = "aqmapr"),
      as_reference = use_references
    ) |>
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
      url = "https://cwfis.cfs.nrcan.gc.ca/geoserver/ows",
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
      network_type = factor(
        monitor_type,
        levels = c("FEM", "PA", "EGG"),
        labels = c("Regulatory (FEM)", "PurpleAir (PA)", "AQegg (EGG)")
      ),
    ) |>
    dplyr::select(dplyr::all_of(desired_cols)) |>
    sf::st_as_sf(coords = c("lng", "lat"))
}
