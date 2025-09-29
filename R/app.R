#' Start the Ambiorix server
#'
#' Start the Ambiorix server and listen on the specified port and host.
#'
#' @param background (Optional).
#'   Should the server be run in the background?
#'   If `TRUE`, you must assign the output to a variable to maintain the connection (see [callr::r_bg]).
#'   Default is `FALSE`.
#'
#' @export
start_server <- function(background = FALSE) {
  if (background) {
    stop("Not yet implemented")
    rlang::check_installed("mirai")
    rlang::check_installed("aqmapr")
    mirai::daemons(1)
    return(
      mirai::mirai({
        library(aqmapr)
        start_server(background = FALSE)
      })
    )
  }
  .cst <- load_constants()

  app <- ambiorix::Ambiorix$new(
    port = .cst$server$port,
    host = .cst$server$host
  )

  ## Serve icons and js
  app$static(.cst$icon_dir$local, .cst$icon_dir$server)
  app$static(.cst$js_dir$local, .cst$js_dir$server)

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
    out_data <- load_recent_aqmap_data(
      aqmap_url = .cst$aqmap_url,
      data_dir = .cst$data_dir,
      desired_cols = .cst$recent_data_cols,
      allowed_networks = .cst$allowed_networks
    ) |>
      handyr::on_error(.return = NULL)
  } else if (name == "plotting") {
    out_data <- load_aqmap_plot_data(
      network = network,
      site_id = site_id,
      aqmap_url = .cst$aqmap_url,
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

  recent_aqmap_data <- load_recent_aqmap_data(
    aqmap_url = .cst$aqmap_url,
    data_dir = .cst$data_dir,
    desired_cols = .cst$recent_data_cols,
    allowed_networks = .cst$allowed_networks
  ) |>
    handyr::on_error(.return = NULL)

  map <- make_aqmap(
    marker_data = recent_aqmap_data,
    base_maps = .cst$base_maps,
    template_dir = .cst$image_dir,
    icon_dirs = .cst$icon_dir,
    js_dirs = .cst$js_dir,
    font_sizes = .cst$font_sizes,
    marker_sizes = .cst$marker_sizes,
    pm25_units = .cst$units$pm25,
    text = .cst$text,
    force_update_icons = .cst$force_update_icons
  )

  res$htmlwidget(map)
}
