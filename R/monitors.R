add_obs_markers <- function(map, marker_data) {
  hover_options <- leaflet::labelOptions(
    sticky = FALSE,
    textOnly = FALSE,
    opacity = 0.9,
    offset = c(15, 0),
    direction = "right"
  )

  # Make monitor icons based on that networks mean
  network_means <- marker_data |>
    dplyr::group_by(network) |>
    dplyr::summarise(pm25_1hr = mean(pm25_1hr, na.rm = TRUE))
  unique_networks <- levels(marker_data$network) |>
    make_icon_svg(
      pm25_1hr = network_means$pm25_1hr,
      force = TRUE
    )

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
        make_marker_icon_path(pm25_1hr = pm25_1hr),
      # Build hover label
      label = make_monitor_hover(
        name = name,
        network = network,
        date_last_obs = date_last_obs,
        pm25_10min = pm25_10min,
        pm25_1hr = pm25_1hr,
        pm25_3hr = pm25_3hr,
        pm25_24hr = pm25_24hr
      )
    )

  # Add markers to map - 1 pane for missing, 1 for not
  map <- map |>
    leaflet::addMapPane(names(.marker_sizes)[1], zIndex = 415) |>
    leaflet::addMapPane(names(.marker_sizes)[2], zIndex = 420) |>
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

add_monitor_legend = function(
  map,
  networks,
  legend_details = list(
    hover = "Monitor Types",
    title = "Monitor Types"
  ),
  position = "bottomright"
) {
  # Make icon paths
  network_icons <- networks |>
    make_marker_icon_path(
      pm25_1hr = rep(-1, length(networks)),
      local = FALSE
    ) |>
    setNames(networks)

  # Make legend title
  title <- legend_details$title |>
    htmltools::tags$strong() |>
    htmltools::tags$span(title = legend_details$hover)

  # Make icon references
  style <- "vertical-align: middle; max-width: %spx; max-height: %spx;" |>
    sprintf(.marker_sizes$legend, .marker_sizes$legend)
  icons <- network_icons |>
    handyr::for_each(
      \(pth) htmltools::tags$img(src = pth, style = style),
      .name = TRUE,
      .quiet = TRUE
    )

  # Make text for beside each icon
  style <- "vertical-align: middle;"
  texts <- networks |>
    pretty_text() |>
    handyr::for_each(htmltools::tags$span, style = style, .name = TRUE)

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
