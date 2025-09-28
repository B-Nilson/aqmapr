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
    font_sizes = .cst$font_sizes,
    marker_sizes = .cst$marker_sizes,
    pm25_units = .cst$units$pm25,
    text = .cst$text,
    force_update_icons = .cst$force_update_icons
  )

  res$htmlwidget(map)
}
