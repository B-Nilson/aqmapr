test_that("add_obs_markers() works", {
  stop("Requires server to be running")
  map <- leaflet::leaflet()
  networks <- c("agency", "lcm", "purpleair", "aqegg")
  map |>
    add_obs_markers(networks = networks) |>
    expect_no_error()
})

test_that("add_monitor_legend() works", {
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
