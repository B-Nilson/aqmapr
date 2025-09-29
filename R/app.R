#' Start a local Ambiorix server and serve the AQmapr app
#'
#' @description
#' Start the Ambiorix server and listen on the specified port and host.
#' See `host`:`port` (i.e. 127.0.0.1:8000) in your browser to view app once started.
#' Use `Ctrl+C` to stop the server.
#'
#' The following endpoints are available:
#' - `GET /`: AQmapr main page (see [aqmapr::make_aqmap])
#' - `GET /data/recent/:type`: Recent AQmap data (see [load_recent_aqmap_data]) as type= json, csv, or tsv
#' - `GET /data/meta/:type`: A subset of the recent AQmap data with only metadata as type= json, csv, or tsv
#' - `GET /data/plotting/:network/:site_id`: Historic site data (see [aqmapr::load_aqmap_plot_data]) as type= json, csv, or tsv
#' - `GET /css`: AQmapr css
#' - `GET /js`: AQmapr js
#' - `GET /icons`: AQmapr icons
#'
#' @param host (Optional).
#'   The host to listen on.
#'   Default is "127.0.0.1" (localhost).
#' @param port (Optional).
#'   The port to listen on.
#'   Default is 8000.
#' @param icon_dir,css_dir,js_dir (Optional).
#'   The directory where AQmapr icons/css/js are stored.
#'   Default points to relevant directories in the `aqmapr` package installation.
#'   These should not need to be changed.
#'
#' @export
start_server <- function(
  host = "127.0.0.1",
  port = 8000,
  icon_dir = system.file("images/icons", package = "aqmapr"),
  css_dir = system.file("css", package = "aqmapr"),
  js_dir = system.file("js", package = "aqmapr")
) {
  # Initialize Ambiorix server object
  app <- ambiorix::Ambiorix$new(port = port, host = host)

  ## Serve icons css, and js
  app$static(icon_dir, "/icons")
  app$static(css_dir, "/css")
  app$static(js_dir, "/js")

  ## Serve data
  # i.e. /data/recent/json
  app$get("/data/:name/:type", get_data)

  # i.e. /data/plotting/agency/10102/json
  app$get("/data/:name/:network/:site_id/:type", get_data)

  ## Serve map
  app$get("/", get_map)

  ## Start server
  app$start()
}

# Ambiorix data GET handler (for /data/:name/:type...)
get_data <- function(req, res) {
  allowed_types <- c("json", "csv", "tsv") # first is default
  name <- req$params$name
  type <- req$params$type
  network <- req$params$network # for name == "plotting"
  site_id <- req$params$site_id # for name == "plotting"

  if (!".cst" %in% ls()) {
    .cst <- load_constants()
  }

  # Set default type if not specified
  if (is.null(type)) {
    type <- allowed_types[1]
  }

  # Load requested data
  if (name %in% c("recent", "meta")) {
    out_data <- load_recent_aqmap_data() |>
      handyr::on_error(.return = NULL)
  } else if (name == "plotting") {
    out_data <- load_aqmap_plot_data(
      network = network,
      site_id = site_id,
      allowed_networks = .cst$allowed_networks
    ) |>
      handyr::on_error(.return = NULL)
  } else {
    out_data <- NULL
  }

  # Create secondary looks if needed
  if (name == "meta") {
    out_data <- out_data |>
      dplyr::select(-dplyr::starts_with(c("pm25_", "date"))) |>
      handyr::on_error(.return = NULL)
  }

  # Return response if valid request (404 if not)
  if (type %in% allowed_types) {
    res[[type]](out_data)
  } else {
    res$not_found()
  }
}

# Ambiorix map GET handler (for /)
get_map <- function(req, res) {
  if (!".cst" %in% ls()) {
    .cst <- load_constants()
  }

  recent_aqmap_data <- load_recent_aqmap_data() |>
    handyr::on_error(.return = NULL)

  map <- make_aqmap(
    marker_data = recent_aqmap_data,
    base_maps = .cst$base_maps,
    font_sizes = .cst$font_sizes,
    marker_sizes = .cst$marker_sizes,
    pm25_units = .cst$units$pm25,
    monitor_hover_text = .cst$text$monitor_hover,
    monitor_legend_title = .cst$text$monitor_legend$title |> 
      stats::setNames(.cst$text$monitor_legend$hover),
    force_update_icons = .cst$force_update_icons
  )

  res$htmlwidget(map)
}
