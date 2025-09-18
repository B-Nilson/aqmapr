# Ambiorix data GET handler (for /data/:name/:type...)
get_data <- function(req, res) {
  allowed_types <- c("json", "csv", "tsv") # first is default
  name <- req$params$name
  type <- req$params$type
  network <- req$params$network # for name == "plotting"
  site_id <- req$params$site_id # for name == "plotting"

  # Set default type if not specified
  if (is.null(type)) {
    type <- allowed_types[1]
  }

  # Load requested data
  if (name == "recent") {
    out_data <- load_recent_aqmap_data() |>
      handyr::on_error(.return = NULL)
  } else if (name == "plotting") {
    out_data <- load_aqmap_plot_data(network = network, site_id = site_id) |>
      handyr::on_error(.return = NULL)
  } else if (name == "meta") {
    out_data <- load_aqmap_meta_data() |>
      handyr::on_error(.return = NULL)
  }else {
    out_data <- NULL
  }

  # Return response if valid request (404 if not)
  if (type %in% allowed_types) {
    res[[type]](out_data)
  }
}
