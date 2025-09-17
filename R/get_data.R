# Ambiorix data GET handler (for /data/:name/:type...)
get_data <- function(req, res) {
  allowed_types <- c("json", "csv", "tsv") # first is default
  name <- req$params$name
  type <- req$params$type

  # Set default type if not specified
  if (is.null(type)) {
    type <- allowed_types[1]
  }

  # Load requested data
  if (name == "recent") {
    out_data <- load_recent_aqmap_data() |>
      handyr::on_error(.return = NULL)
  }else {
    out_data <- NULL
  }

  # Return response if valid request (404 if not)
  if (type %in% allowed_types) {
    res[[type]](out_data)
  }
}