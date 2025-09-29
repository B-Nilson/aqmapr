test_that("add_obs_markers() works", {
  skip("Requires server to be running")
  .cst <- load_constants()
  map <- leaflet::leaflet()
  networks <- names(.cst$allowed_networks)
  marker_data <- load_recent_aqmap_data(
    aqmap_url = .cst$aqmap_url,
    data_dir = .cst$data_dir,
    desired_cols = .cst$recent_data_cols,
    allowed_networks = .cst$allowed_networks
  ) |>
    dplyr::filter(
      !is.na(pm25_1hr),
      date_last_obs >= Sys.time() - lubridate::hours(3)
    ) |>
    dplyr::distinct(monitor_type, .keep_all = TRUE)

  map |>
    add_obs_markers(
      marker_data = marker_data,
      template_dir = .cst$image_dir,
      icon_dir = .cst$icon_dir$local,
      font_sizes = .cst$font_sizes$markers,
      marker_sizes = .cst$marker_sizes,
      pm25_units = .cst$units$pm25,
      marker_hover_text = .cst$text$monitor_hover,
      force_update_icons = .cst$force_update_icons
    ) |>
    expect_no_error()
})

test_that("add_monitor_legend() works", {
  skip("Requires server to be running")
  .cst <- load_constants()
  map <- leaflet::leaflet()
  networks <- names(.cst$allowed_networks)

  map |>
    add_monitor_legend(
      networks = networks,
      legend_title = .cst$text$monitor_legend$title |> 
        stats::setNames(.cst$text$monitor_legend$hover),
      icon_dir = .cst$icon_dir$server,
      css_dir = .cst$css_dir$local,
      css_endpoint = .cst$css_dir$server,
      position = "bottomright"
    ) |>
    expect_no_error()
})
