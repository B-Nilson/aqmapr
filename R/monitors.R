add_obs_markers <- function(
  map,
  marker_data,
  template_dir,
  icon_dir,
  font_sizes,
  marker_sizes,
  pm25_units,
  marker_hover_text,
  force_update_icons = FALSE
) {
  stopifnot("leaflet" %in% class(map))
  stopifnot(is.data.frame(marker_data))
  stopifnot(
    is.list(marker_sizes),
    length(marker_sizes) == 3,
    all(c("missing", "obs", "legend") %in% names(marker_sizes))
  )
  stopifnot(is.character(pm25_units), length(pm25_units) == 1)
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
  unique_networks <- levels(marker_data$network) |>
    make_icon_svg(
      pm25_1hr = network_means$pm25_1hr,
      template_dir = template_dir,
      icon_dir = icon_dir,
      font_sizes = font_sizes,
      marker_sizes = marker_sizes,
      force = TRUE
    )

  # Ensure icons exist
  marker_data$network |>
    as.character() |>
    make_icon_svg(
      pm25_1hr = marker_data$pm25_1hr,
      template_dir = template_dir,
      icon_dir = icon_dir,
      font_sizes = font_sizes,
      marker_sizes = marker_sizes,
      force = force_update_icons
    )

  # Add helper columns
  marker_data <- marker_data |>
    dplyr::mutate(
      # Determine pane to use based on pm25_1hr missing or not
      pane = is.na(.data$pm25_1hr) |>
        ifelse(names(marker_sizes)[1], names(marker_sizes)[2]),
      # Select icon size similarily - smaller for missing obs
      icon_width = unname(unlist(marker_sizes[.data$pane])),
      icon_height = .data$icon_width,
      # Build url to icon
      icon_url = .data$network |>
        as.character() |>
        make_marker_icon_path(pm25_1hr = .data$pm25_1hr, icon_dir = icon_dir),
      # Build hover label
      label = make_monitor_hover(
        name = .data$name,
        network = .data$network,
        date_last_obs = .data$date_last_obs,
        pm25_10min = .data$pm25_10min,
        pm25_1hr = .data$pm25_1hr,
        pm25_3hr = .data$pm25_3hr,
        pm25_24hr = .data$pm25_24hr,
        pm25_units = pm25_units,
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
  legend_details = list(
    hover = "Hover text",
    title = "Legend Title"
  ),
  icon_dir,
  marker_size,
  position = "bottomright"
) {
  stopifnot("leaflet" %in% class(map))
  stopifnot(is.character(networks), length(networks) > 0)
  stopifnot(
    is.list(legend_details),
    length(legend_details) == 2,
    all(c("hover", "title") %in% names(legend_details))
  )
  stopifnot(is.character(icon_dir), length(icon_dir) == 1)
  stopifnot(is.numeric(marker_size), length(marker_size) == 1)
  stopifnot(
    is.character(position),
    length(position) == 1,
    position %in% c("bottomright", "bottomleft", "topleft", "topright")
  )

  # Make icon paths
  network_icons <- networks |>
    make_marker_icon_path(
      pm25_1hr = rep(-1, length(networks)),
      icon_dir = icon_dir
    ) |>
    stats::setNames(networks)

  # Make legend title
  title <- legend_details$title |>
    htmltools::tags$strong() |>
    htmltools::tags$span(title = legend_details$hover)

  # Make icon references
  style <- "vertical-align: middle; max-width: %spx; max-height: %spx;" |>
    sprintf(marker_size, marker_size)
  icons <- network_icons |>
    lapply(
      \(pth) htmltools::tags$img(src = pth, style = style)
    ) |>
    stats::setNames(networks)

  # Make text for beside each icon
  style <- "vertical-align: middle;"
  texts <- networks |>
    pretty_text() |>
    lapply(htmltools::tags$span, style = style) |>
    stats::setNames(networks)

  # Combine tags
  legend <- title |>
    htmltools::tags$div(
      htmltools::tags$br(),
      icons$agency,
      texts$agency,
      htmltools::tags$br(),
      icons$lcm,
      texts$lcm,
    )

  # Add to map
  map |>
    leaflet::addControl(
      html = legend,
      layerId = "monitor_legend",
      position = position
    )
}
