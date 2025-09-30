add_obs_markers <- function(
  map,
  marker_data,
  font_sizes,
  marker_sizes = list(missing = 17, obs = 33),
  marker_hover_text,
  template_dir = system.file("images", package = "aqmapr"),
  icon_dir = system.file("images/icons", package = "aqmapr"),
  force_update_icons = FALSE
) {
  stopifnot("leaflet" %in% class(map))
  stopifnot(is.data.frame(marker_data))
  stopifnot(
    is.list(marker_sizes),
    length(marker_sizes) == 2,
    all(c("missing", "obs") %in% names(marker_sizes))
  )
  stopifnot(is.list(marker_hover_text), length(marker_hover_text) > 0)
  stopifnot(is.logical(force_update_icons), length(force_update_icons) == 1)

  hover_options <- leaflet::labelOptions(
    sticky = FALSE,
    textOnly = FALSE,
    opacity = 0.9,
    offset = c(15, 0),
    direction = "right"
  )
  # Make monitor icons based on that networks mean
  network_means <- marker_data |>
    dplyr::group_by(.data$network) |>
    dplyr::summarise(pm25_1hr = mean(.data$pm25_1hr, na.rm = TRUE))
  levels(marker_data$network) |>
    make_icon_svg(
      values = network_means$pm25_1hr,
      template_dir = template_dir,
      icon_dir = icon_dir,
      font_sizes = font_sizes,
      marker_size = marker_sizes$obs,
      marker_size_missing = marker_sizes$missing,
      for_legend = TRUE,
      force = TRUE
    )

  # Ensure icons exist
  marker_data$network |>
    as.character() |>
    make_icon_svg(
      values = marker_data$pm25_1hr,
      template_dir = template_dir,
      icon_dir = icon_dir,
      font_sizes = font_sizes,
      marker_size = marker_sizes$obs,
      marker_size_missing = marker_sizes$missing,
      force = force_update_icons
    )

  # Add helper columns
  marker_data <- marker_data |>
    dplyr::mutate(
      # Determine pane to use based on pm25_1hr missing or not
      pane = names(marker_sizes)[(!is.na(.data$pm25_1hr)) + 1],
      # Select icon size similarily - smaller for missing obs
      icon_width = unname(unlist(marker_sizes[.data$pane])),
      icon_height = .data$icon_width,
      # Build url to icon
      icon_url = .data$network |>
        as.character() |>
        make_icon_path(values = .data$pm25_1hr, icon_dir = icon_dir),
      # Build hover label
      label = make_monitor_hover(
        name = .data$name,
        network = .data$network,
        date_last_obs = .data$date_last_obs,
        pm25_10min = .data$pm25_10min,
        pm25_1hr = .data$pm25_1hr,
        pm25_3hr = .data$pm25_3hr,
        pm25_24hr = .data$pm25_24hr,
        text = marker_hover_text
      )
    )

  # Add markers to map - 1 pane for missing, 1 for not
  map |>
    leaflet::addMapPane(names(marker_sizes)[1], zIndex = 415) |>
    leaflet::addMapPane(names(marker_sizes)[2], zIndex = 420) |>
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
      ),
      label = ~ label |> lapply(htmltools::HTML),
      labelOptions = hover_options
    )
}

add_monitor_legend <- function(
  map,
  networks,
  legend_title,
  icon_dir = system.file("images/icons", package = "aqmapr"),
  css_dir = system.file("css", package = "aqmapr"),
  css_endpoint = "/css",
  position = "bottomright"
) {
  stopifnot("leaflet" %in% class(map))
  stopifnot(is.character(networks), length(networks) > 0)
  stopifnot(is.character(legend_title), length(legend_title) == 1)
  stopifnot(is.character(icon_dir), length(icon_dir) == 1)
  stopifnot(is.character(css_dir), length(css_dir) == 1)
  stopifnot(
    is.null(css_endpoint) |
      (is.character(css_endpoint) & length(css_endpoint) == 1)
  )
  if (is.null(css_endpoint)) {
    # Assume files available locally if no endpoint
    css_endpoint <- css_dir
  }
  stopifnot(
    is.character(position),
    length(position) == 1,
    position %in% c("bottomright", "bottomleft", "topleft", "topright")
  )

  # Ensure css file exists
  css_file <- "monitor_legend.css"
  css_local <- file.path(css_dir, css_file)
  css_server <- file.path(css_endpoint, css_file)
  stopifnot(file.exists(css_local))

  # Make legend title
  title_tag <- legend_title |>
    htmltools::HTML() |>
    htmltools::tags$strong() |>
    htmltools::tags$span(title = names(legend_title))

  # Make icon paths
  icon_paths <- networks |>
    make_icon_path(
      values = NA_real_,
      icon_dir = icon_dir,
      for_legend = TRUE
    )

  # Make icon img tags
  icon_tags <- icon_paths |>
    lapply(
      \(pth) htmltools::tags$img(src = pth, class = "legend-icon")
    ) |>
    stats::setNames(networks)

  # Make text for beside each icon
  text_tags <- pretty_text(networks) |>
    lapply(htmltools::tags$span, class = "legend-labels") |>
    stats::setNames(networks)

  # Combine each networks tags
  legend_entries <- networks |>
    lapply(\(network) {
      icon_tags[[network]] |>
        htmltools::tags$div(text_tags[[network]])
    })

  # Combine tags into legend
  legend_tag <- title_tag |>
    htmltools::tags$div(legend_entries)

  # Add to map and include relevant css
  map |>
    leaflet::addControl(
      html = legend_tag,
      layerId = "monitor-legend",
      position = position
    ) |>
    include_scripts(paths = css_server, types = "css")
}
