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
