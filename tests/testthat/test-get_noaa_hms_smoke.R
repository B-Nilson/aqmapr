test_that("basic case works", {
  hms <- get_noaa_hms_smoke(quiet = TRUE) |>
    expect_no_error() |>
    expect_no_warning()

  expect_equal(
    names(hms),
    c("satellite", "period", "density", "geometry")
  )
})
