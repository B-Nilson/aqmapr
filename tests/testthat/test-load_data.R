test_that("load_recent_aqmap_data() works", {
  .cst <- load_constants()
  data_dir <- tempdir()
  result <- load_recent_aqmap_data(data_dir = data_dir)

  # Copy of data saved where expected
  expect_true(file.exists(file.path(data_dir, "aqmap_most_recent_obs.Rds")))

  # Returned data is correct
  expected_cols <- c(
      "site_id",
      "name",
      "network",
      "monitor_type", # (un-grouped network - i.e PA or EGG instead of lcm)
      "lat",
      "lng",
      "prov_terr",
      "date_last_obs",
      "pm25_10min",
      "pm25_1hr",
      "pm25_3hr",
      "pm25_24hr"
    )
  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), expected_cols)
  expect_true(nrow(result) > 0)
})

test_that("load_aqmap_plot_data() works", {
  .cst <- load_constants()
  meta <- load_recent_aqmap_data() |>
    dplyr::filter(
      !is.na(pm25_1hr),
      date_last_obs >= Sys.time() - lubridate::hours(3)
    ) |>
    dplyr::distinct(monitor_type, .keep_all = TRUE)

  result <- meta$monitor_type |>
    as.character() |> 
    purrr::map2(
      meta$site_id,
      ~ load_aqmap_plot_data(
        network = .x,
        site_id = .y,
        allowed_networks = .cst$allowed_networks
      )
    ) |>
    stats::setNames(meta$monitor_type)

  # Returned data is correct
  for (network in meta$monitor_type) {
    expect_s3_class(result[[network]], "data.table")
    expect_true(nrow(result[[network]]) > 0)
    expect_true("pm25" %in% names(result[[network]]))
  }
})
