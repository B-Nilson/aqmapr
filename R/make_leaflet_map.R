#' Simplify making a leaflet map
#'
#' @export
#' @param base_maps (Optional).
#'   A character vector named basemaps from [leaflet::providers] to add to the map.
#'   Names will be used in the control menu for the basemaps if provided.
#'   Default is a nice light and dark open data theme.
#' @param point_layers,polygon_layers,wms_layers (Optional).
#'   A list of 1 or more `PointLayer`/`PolygonLayer`/`WMSLayer` objects (created with [PointLayer()]/[PolygonLayer()]/[WMSLayer()]) to be added to the map.
#'   Default is an empty list (no points/polygons/WMS layers added).
#' @param page_title (Optional).
#'   A character string of the title to display in the browser tab when the map is saved to an HTML file.
#'   Default `NULL` (no title).
#' @param layer_control_titles (Optional).
#'   A 2-length character vector to use for the basemap and layer control titles.
#'   Default is c("Basemaps", "Layers").
#' @param attribution (Optional).
#'   A character string of the attribution text (HTML supported) to display in the bottom right of the map alongside the basemap attribution.
#'   Default is `NULL` (no attribution).
#' @param center_on_opened_popup (Optional).
#'   If TRUE, the map will be centered on the popup when it is opened.
#'   Default is FALSE.
#' @param track_map_state (Optional).
#'   If TRUE, the map center and zoom will be tracked and saved in the URL when the map is saved to an HTML file.
#'   This allows the map to be loaded with the same state on page load/refresh.
#'   Default is FALSE.
#' @param include_timestamp (Optional).
#'   If TRUE, the current timestamp (browser time) will be included in a bottom left leaflet control.
#'   If a single POSIXct object is passed, it will be used as the timestamp instead of the current time.
#'   Default is FALSE.
#' @param include_scalebar (Optional).
#'   If `TRUE`, a scale bar will be included in the bottom left leaflet control.
#'   If instead a character equal to
#'   `"bottomright"`, `"topleft"`, `"bottomleft"`, or `"topright"`
#'   is passed, it will be used as the position of the scale bar.
#'   Default is `TRUE`.
#' @param as_reference (Optional). If TRUE, js/css will be referenced in the map header. If FALSE, the js/css will be embeded directly in the map.
#'   Requires local server to be running (see [start_server()]), or the js and css files need to be hosted in "/js" and "/css" respectively relative to the html file.
#'   Run `system.file("js", package = "aqmapr")`/`system.file("css", package = "aqmapr")` to find the location of the js/css files respectively.
#'   Default is FALSE.
#' @return A leaflet map object.
#' @importFrom rlang exec !!!
#' @examples
#'
#' colour_pal <- leaflet::colorFactor(
#'   "viridis",
#'   domain = levels(canada_communities$type),
#'   ordered = TRUE,
#'   reverse = TRUE
#' )
#' point_layers <- list(PointLayer(
#'   group = "Communities",
#'   data = canada_communities,
#'   fill_palette = colour_pal,
#'   fill = ~type,
#'     label = ~ paste("Name: ", name, "<br/>", "Type: ", type) |>
#'       lapply(htmltools::HTML)
#' ))
#' make_leaflet_map(point_layers = point_layers)
#'
#' make_leaflet_map(
#'   polygon_layers = list(PolygonLayer(
#'     group = "Provinces",
#'     data = canadata::provinces_and_territories,
#'     fill = "black",
#'     opacity = 0.1,
#'     label = ~name
#'   ))
#' )
make_leaflet_map <- function(
  base_maps = c(
    "Light Theme" = "OpenStreetMap",
    "Dark Theme" = "CartoDB.DarkMatter"
  ),
  point_layers = list(),
  polygon_layers = list(),
  wms_layers = list(),
  page_title = NULL,
  layer_control_titles = c("Basemaps", "Layers"),
  attribution = NULL,
  center_on_opened_popup = FALSE,
  track_map_state = FALSE,
  include_timestamp = FALSE,
  include_scalebar = TRUE,
  as_reference = FALSE
) {
  stopifnot(
    inherits(point_layers, "list"),
    length(point_layers) == 0 |
      all(sapply(point_layers, \(layer) S7::S7_inherits(layer, PointLayer))),
    inherits(polygon_layers, "list"),
    length(polygon_layers) == 0 |
      all(sapply(polygon_layers, \(layer) {
        S7::S7_inherits(layer, PolygonLayer)
      })),
    inherits(wms_layers, "list"),
    length(wms_layers) == 0 |
      all(sapply(wms_layers, \(layer) S7::S7_inherits(layer, WMSLayer))),
    is.logical(track_map_state),
    length(track_map_state) == 1,
    is.logical(as_reference),
    length(as_reference) == 1
  )

  # Make basemap
  base_map <- base_maps |>
    make_base_map(
      page_title = page_title,
      layer_control_titles = layer_control_titles,
      attribution = attribution,
      include_scalebar = include_scalebar,
      include_timestamp = include_timestamp,
      as_reference = as_reference
    ) |>
    # Insert js to define layer names for other js functions
    include_layers_js(
      base_maps = base_maps,
      point_layers = point_layers,
      polygon_layers = polygon_layers,
      wms_layers = wms_layers
    )

  # Center map on popups when they are opened if desired
  if (center_on_opened_popup) {
    base_map <- base_map |> center_on_opened_popup(as_reference = as_reference)
  }

  # Add layers as needed
  for (layer in c(point_layers, polygon_layers, wms_layers)) {
    base_map <- base_map |> add_to_map(layer = layer)
  }

  # track map location/layers/basemap
  if (track_map_state) {
    base_map <- base_map |>
      track_map_state(as_reference = as_reference)
  }

  return(base_map)
}

make_base_map <- function(
  base_maps,
  page_title = NULL,
  layer_control_titles = c("Basemaps", "Layers"),
  attribution = NULL,
  include_timestamp = FALSE,
  include_scalebar = TRUE,
  as_reference = FALSE
) {
  stopifnot(
    is.character(base_maps),
    length(base_maps) > 0,
    length(names(base_maps)) == length(base_maps),
    is.logical(include_timestamp) | lubridate::is.POSIXct(include_timestamp),
    length(include_timestamp) == 1,
    is.logical(include_scalebar) | is.character(include_scalebar),
    length(include_scalebar) == 1
  )
  base_map <- leaflet::leaflet() |>
    add_base_maps(base_maps = base_maps) |>
    add_control_titles(
      base_title = layer_control_titles[1],
      layers_title = layer_control_titles[2],
      as_reference = as_reference
    ) |>
    set_page_title(page_title = page_title) |>
    add_attribution(attribution, sep = " | ") |>
    leaflet.extras::addFullscreenControl() |>
    add_locator_button() |>
    leaflet.extras::addResetMapButton() |>
    leaflet.extras::enableTileCaching()

  # Add a timestamp to bottom left if desired
  if (include_timestamp) {
    if (is.logical(include_timestamp)) {
      include_timestamp <- Sys.time()
    }
    base_map <- base_map |>
      add_map_timestamp(
        timestamp = include_timestamp,
        as_reference = as_reference
      )
  }

  # Add scalebar if desired
  if (include_scalebar) {
    allowed_positions <- c("topleft", "topright", "bottomleft", "bottomright")
    position <- (include_scalebar %in% allowed_positions) |>
      ifelse(include_scalebar, "bottomleft")
    base_map <- base_map |>
      leaflet::addScaleBar(position = position)
  }

  return(base_map)
}

add_fill_legend <- function(
  map,
  data,
  fillColor,
  palette,
  group = NULL,
  title = group,
  opacity = 0.5,
  position = "bottomleft",
  na_label = "No Data"
) {
  # Handle NULL values
  if (is.null(position)) {
    position <- "bottomleft"
  }
  if (is.null(opacity)) {
    opacity <- 0.5
  }

  fill_values <- data |>
    dplyr::pull(!!rlang::as_quosure(fillColor))

  map |>
    leaflet::addLegend(
      title = title,
      group = group,
      pal = palette,
      values = fill_values,
      position = position,
      opacity = opacity,
      na.label = "No Data"
    )
}
