test_that("add_obs_markers() works", {
  skip("Requires server to be running")
  map <- leaflet::leaflet()
  networks <- c("agency", "lcm", "purpleair", "aqegg")
  marker_data <- load_recent_aqmap_data() |>
    dplyr::filter(
      !is.na(pm25_1hr),
      date_last_obs >= Sys.time() - lubridate::hours(3)
    ) |>
    dplyr::distinct(monitor_type, .keep_all = TRUE)

  map |>
    add_obs_markers(marker_data = marker_data) |>
    expect_no_error()
})

test_that("add_monitor_legend() works", {
  skip("Requires server to be running")
  map <- leaflet::leaflet()
  networks <- c("agency", "lcm", "purpleair", "aqegg")

  map |>
    add_monitor_legend(
      networks = networks,
      legend_title = c("test" = "test"),
      position = "bottomright"
    ) |>
    expect_no_error()
})
