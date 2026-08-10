test_that("basic case works", {
  eer <- get_eccc_eer_smoke(quiet = TRUE) |>
    expect_no_error() |>
    expect_no_warning()

  expect_equal(
    names(eer),
    c(
      "region",
      "model_time",
      "forecast_time",
      "min_pm25",
      "altitude",
      "geometry"
    )
  )
})

test_that("make_eer_zip_dir builds correct urls", {
  today <- lubridate::today(tzone = "UTC")

  # Current day -> top-level "latest" alias
  todays_run <- as.POSIXct(format(today, "%Y-%m-%d 06:00:00"), tz = "UTC")
  expect_identical(
    make_eer_zip_dir(todays_run),
    "https://eer.cmc.ec.gc.ca/mandats/AutoSim/Fire/latest/Canada/latest/shp"
  )

  # Archived run within the rolling window -> cycle + dated path
  archived_run <- as.POSIXct(format(today - 1, "%Y-%m-%d 12:00:00"), tz = "UTC")
  expect_identical(
    make_eer_zip_dir(archived_run),
    sprintf(
      "https://eer.cmc.ec.gc.ca/mandats/AutoSim/Fire/12UTC/Canada/%s.1200/shp",
      format(today - 1, "%Y%m%d")
    )
  )

  # Run older than the archive window -> friendly error
  old_run <- as.POSIXct(format(today - 9, "%Y-%m-%d 12:00:00"), tz = "UTC")
  expect_error(
    make_eer_zip_dir(old_run),
    "only archived for the most recent"
  )
})
