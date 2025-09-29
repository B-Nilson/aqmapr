test_that("output map has not changed", {
  skip("Requires server to be running")
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

  map <- make_aqmap(marker_data = test_aqmap_data) |>
    expect_no_error() |>
    expect_no_warning()
  expect_snapshot(map)
})
