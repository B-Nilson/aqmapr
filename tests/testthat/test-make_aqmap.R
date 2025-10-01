test_that("output map has not changed", {
  skip("Requires server to be running")

  map <- make_aqmap(networks = c("agency", "lcm")) |>
    expect_no_error() |>
    expect_no_warning()
  expect_snapshot(map)
})
