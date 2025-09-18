# Create leaflet map similiar to AQmap
make_leaflet_map <- function(marker_data = NULL) {
  map <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap)

  if (!is.null(marker_data)) {
    map <- map |>
      add_obs_markers(marker_data = marker_data) |>
      add_monitor_legend(
        networks = levels(marker_data$network),
        legend_details = list(
          hover = "Fine particulate matter monitor types. Values are in units of &mu;g m<sup>-3</sup>, colour coded using the Canadian AQHI+ system.",
          title = htmltools::HTML("PM<sub>2.5</sub> Monitors")
        )
      ) |>
      leaflet::addLayersControl(
        overlayGroups = levels(marker_data$network) |>
          pretty_text()
      )
  }

  return(map)
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
