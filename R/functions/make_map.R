# Create leaflet map similiar to AQmap
make_leaflet_map <- function(marker_data = NULL) {
  map <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap)

  if (!is.null(marker_data)) {
    map <- map |>
      add_obs_markers(marker_data = marker_data) |>
      add_monitor_legend(
        networks = marker_data$network,
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
  network_icons <- levels(networks) |>
    make_marker_icon_path(pm25_1hr = rep(-1, length(levels(networks)))) |>
    stringr::str_remove("../images") |> # make relative
    setNames(levels(networks))

  # Make legend title
  title <- htmltools::tags$span(
    title = legend_details$hover,
    htmltools::tags$strong(legend_details$title)
  )

  # Make icon references
  style <- "vertical-align: middle; max-width: 22px; max-height: 22px;"
  icons <- as.character(network_icons) |>
    lapply(\(pth) htmltools::tags$img(src = pth, style = style)) |>
    setNames(names(network_icons))

  # Make text for beside each icon
  style <- "vertical-align: middle;"
  texts <- levels(networks) |>
    lapply(\(network) {
      htmltools::tags$span(
        style = style,
        pretty_text(network)
      )
    }) |>
    setNames(levels(networks))

  # Combine and return
  m_type_legend <- htmltools::tags$div(
    title,
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
      m_type_legend,
      layerId = "marker_legend",
      position = position
    )
}
