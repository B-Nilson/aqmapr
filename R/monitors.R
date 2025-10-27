# TODO: add as_reference arg so able to embed icons directly
add_monitor_legend <- function(
  map,
  networks,
  legend_title = "PM<sub>2.5</sub> Monitors",
  icon_dir = system.file("images/icons", package = "aqmapr"),
  icon_endpoint = "/icons",
  css_dir = system.file("css", package = "aqmapr"),
  position = "bottomright"
) {
  stopifnot("leaflet" %in% class(map))
  stopifnot(is.character(networks), length(networks) > 0)
  stopifnot(is.character(legend_title), length(legend_title) == 1)
  stopifnot(is.character(icon_dir), length(icon_dir) == 1)
  stopifnot(is.character(css_dir), length(css_dir) == 1)
  stopifnot(
    is.character(position),
    length(position) == 1,
    position %in% c("bottomright", "bottomleft", "topleft", "topright")
  )

  # Ensure css file exists
  css_path <- css_dir |> file.path("monitor_legend.css")
  stopifnot(file.exists(css_path))

  # Create icons if needed
  networks |>
    make_icon_svg(
      values = rep(0, length(networks)),
      icon_dir = icon_dir,
      for_legend = TRUE,
      force = FALSE
    )

  # Make legend title
  title_tag <- legend_title |>
    htmltools::HTML() |>
    htmltools::tags$strong() |>
    htmltools::tags$span(title = names(legend_title))

  # Make icon paths
  icon_paths <- networks |>
    make_icon_path(
      values = NA_real_,
      icon_dir = icon_endpoint,
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
    include_scripts(paths = css_path, as_reference = FALSE)
}
