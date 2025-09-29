test_that("output map has not changed", {
  .cst <- load_constants()
  test_aqmap_data <- dplyr::tibble(
    network = c("agency", "agency", "lcm", "lcm") |>
      factor(levels = c("agency", "lcm")),
    name = c("name1", "name2", "name3", "name4"),
    date_last_obs = as.POSIXct("2020-01-01 01:00:00", tz = "UTC"),
    lat = 50 + 1:4,
    lng = -160 + 1:4,
    pm25_10min = 0,
    pm25_1hr = 1,
    pm25_3hr = 2,
    pm25_24hr = 3
  )

  map <- make_aqmap(
    marker_data = test_aqmap_data,
    base_maps = .cst$base_maps,
    template_dir = .cst$image_dir,
    icon_dirs = .cst$icon_dir,
    js_dirs = .cst$js_dir,
    css_dirs = .cst$css_dir,
    font_sizes = .cst$font_sizes,
    marker_sizes = .cst$marker_sizes,
    pm25_units = .cst$units$pm25,
    text = .cst$text,
    force_update_icons = .cst$force_update_icons
  ) |>
    expect_no_error() |>
    expect_no_warning()
  expect_snapshot(map)
})
