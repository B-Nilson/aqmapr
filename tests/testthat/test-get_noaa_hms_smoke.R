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

test_that("get_and_unzip_retry self-heals a corrupt cached zip", {
  dir <- tempfile()
  dir.create(dir)

  # A valid source zip containing a shapefile, served via a file:// URL
  src_dir <- file.path(dir, "src")
  dir.create(src_dir)
  shp_name <- "hms_smoke20260101.shp"
  writeLines("geometry data", file.path(src_dir, shp_name))
  src_zip <- file.path(dir, "src.zip")
  zip::zipr(src_zip, files = shp_name, root = src_dir)
  src_url <- paste0("file://", gsub("\\\\", "/", src_zip))

  # Plant a corrupt zip at the cache key
  local_path <- file.path(dir, "cached.zip")
  writeLines("not a zip", local_path)

  paths <- get_and_unzip_retry(
    zip_url = src_url,
    local_path = local_path,
    unzip_dir = file.path(dir, "out"),
    cache = TRUE,
    quiet = TRUE,
    pattern = ".*\\.shp$"
  )
  expect_length(paths, 1)
  expect_true(endsWith(paths, shp_name))
  # The corrupt zip was replaced by a fresh download
  expect_true(file.exists(local_path))
})

test_that("get_and_unzip_retry drops the bad file when the fetch keeps failing", {
  dir <- tempfile()
  dir.create(dir)

  bad_src <- file.path(dir, "bad_src.zip")
  writeLines("garbage source", bad_src)
  local_path <- file.path(dir, "cached.zip")
  writeLines("garbage cache", local_path)

  expect_warning(
    paths <- get_and_unzip_retry(
      zip_url = paste0("file://", gsub("\\\\", "/", bad_src)),
      local_path = local_path,
      unzip_dir = file.path(dir, "out"),
      cache = TRUE,
      quiet = TRUE,
      pattern = ".*\\.shp$"
    ),
    "corrupt"
  )
  expect_length(paths, 0)
  # The bad file is dropped so the next call re-downloads
  expect_false(file.exists(local_path))
})

test_that("corrupt cached zip self-heals on a live call", {
  select_time <- Sys.time()
  shape_date <- select_time |>
    lubridate::with_tz("America/Vancouver") |>
    format("%Y%m%d")
  dir <- tempfile()
  dir.create(dir)

  # Plant a corrupt zip at today's cache key
  local_path <- file.path(dir, sprintf("hms_%s_shp.zip", shape_date))
  writeLines("not a zip", local_path)

  hms <- get_noaa_hms_smoke(select_time = select_time, data_dir = dir, quiet = TRUE)
  expect_no_error(hms)
  expect_false(is.null(hms))
  expect_true(file.exists(local_path))
})
