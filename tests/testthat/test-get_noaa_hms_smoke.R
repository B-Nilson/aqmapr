test_that("basic case works", {
  hms <- get_noaa_hms_smoke(quiet = TRUE) |>
    expect_no_error() |>
    expect_no_warning()

  expect_equal(
    names(hms),
    c("satellite", "period", "density", "geometry")
  )
})

test_that("cache file staleness logic", {
  dir <- tempfile()
  dir.create(dir)
  local_path <- file.path(dir, "hms_test_shp.zip")
  writeLines("test", local_path)

  # Older than the refresh window -> stale (re-download)
  Sys.setFileTime(local_path, Sys.time() - lubridate::dhours(3))
  expect_true(cache_file_stale(local_path, is_todays = TRUE, cache = TRUE, cache_refresh_hours = 1))

  # Fresh -> keep the cached copy
  Sys.setFileTime(local_path, Sys.time())
  expect_false(cache_file_stale(local_path, is_todays = TRUE, cache = TRUE, cache_refresh_hours = 1))

  # Past files never refresh
  expect_false(cache_file_stale(local_path, is_todays = FALSE, cache = TRUE, cache_refresh_hours = 1))

  # Caching disabled -> no staleness check needed
  expect_false(cache_file_stale(local_path, is_todays = TRUE, cache = FALSE, cache_refresh_hours = 1))

  # Inf -> never refresh
  Sys.setFileTime(local_path, Sys.time() - lubridate::dhours(3))
  expect_false(cache_file_stale(local_path, is_todays = TRUE, cache = TRUE, cache_refresh_hours = Inf))

  # Missing file -> nothing cached to refresh
  expect_false(cache_file_stale(file.path(dir, "nope.zip"), is_todays = TRUE, cache = TRUE, cache_refresh_hours = 1))
})
