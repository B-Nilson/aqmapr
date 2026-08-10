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

test_that("clean_eer_files removes only old EER artifacts", {
  dir <- tempfile()
  dir.create(dir)

  # Old EER artifacts (removed)
  old_zip <- file.path(dir, "eer_Canada_20260101-0000_shp.zip")
  old_hour_dir <- file.path(dir, "shp_Canada_20260101-0000")
  old_run_dir <- file.path(dir, "eer_Canada_20260101-0000_shp")
  dir.create(old_hour_dir)
  dir.create(old_run_dir)
  writeLines("x", old_zip)
  writeLines("x", file.path(old_hour_dir, "a.shp"))
  writeLines("x", file.path(old_run_dir, "a.shp"))

  # Fresh EER artifacts (kept)
  new_zip <- file.path(dir, "eer_Canada_20260810-1200_shp.zip")
  new_hour_dir <- file.path(dir, "shp_Canada_20260810-1300")
  new_run_dir <- file.path(dir, "eer_Canada_20260810-1200_shp")
  dir.create(new_hour_dir)
  dir.create(new_run_dir)
  writeLines("x", new_zip)
  writeLines("x", file.path(new_hour_dir, "a.shp"))
  writeLines("x", file.path(new_run_dir, "a.shp"))

  # Non-EER file that must be left alone
  other <- file.path(dir, "hms_20260810_shp.zip")
  writeLines("x", other)

  Sys.setFileTime(old_zip, Sys.time() - lubridate::dhours(48))
  Sys.setFileTime(old_hour_dir, Sys.time() - lubridate::dhours(48))
  Sys.setFileTime(old_run_dir, Sys.time() - lubridate::dhours(48))

  removed <- clean_eer_files(dir, keep_hours = 24)

  expect_setequal(
    basename(removed),
    c(
      "eer_Canada_20260101-0000_shp.zip",
      "shp_Canada_20260101-0000",
      "eer_Canada_20260101-0000_shp"
    )
  )
  expect_false(file.exists(old_zip))
  expect_false(file.exists(old_hour_dir))
  expect_false(file.exists(old_run_dir))
  expect_true(file.exists(new_zip))
  expect_true(file.exists(new_hour_dir))
  expect_true(file.exists(new_run_dir))
  expect_true(file.exists(other))
})
