test_that("basic case works", {
  make_leaflet_map() |>
    add_map_timestamp(
      timestamp = as.POSIXct("2022-01-01 00:00:00", tz = "UTC")
    ) |>
    expect_no_error() |>
    expect_no_warning() |>
    expect_snapshot()
})

test_that("en_francais works", {
  make_leaflet_map() |>
    add_map_timestamp(
      timestamp = as.POSIXct("2022-01-01 00:00:00", tz = "UTC"),
      en_francais = TRUE
    ) |>
    expect_no_error() |>
    expect_no_warning() |>
    expect_snapshot()
})
