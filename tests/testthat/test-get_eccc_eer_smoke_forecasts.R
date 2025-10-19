test_that("basic case works", {
  eer <- get_eccc_eer_smoke_forecasts(quiet = TRUE) |> 
    expect_no_error() |> 
    expect_no_warning()

  expect_equal(
    names(eer),
    c("region", "model_time", "forecast_time", "min_pm25", "altitude", "geometry")
  )
})
