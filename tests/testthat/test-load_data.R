test_that("load_recent_aqmap_data() works", {
  .cst <- load_constants()
  data_dir <- tempdir()
  result <- load_recent_aqmap_data(
    aqmap_url = .cst$aqmap_url,
    data_dir = data_dir,
    desired_cols = .cst$recent_data_cols,
    allowed_networks = .cst$allowed_networks
  )

  # Copy of data saved where expected
  expect_true(file.exists(file.path(data_dir, "aqmap_most_recent_obs.Rds")))

  # Returned data is correct
  expected_cols <- names(.cst$recent_data_cols)
  is_not_named <- expected_cols == ""
  expected_cols[is_not_named] <- .cst$recent_data_cols[is_not_named]
  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), expected_cols)
  expect_true(nrow(result) > 0)
})

test_that("load_aqmap_plot_data() works", {
  .cst <- load_constants()
  meta <- load_recent_aqmap_data(
    aqmap_url = .cst$aqmap_url,
    data_dir = .cst$data_dir,
    desired_cols = .cst$recent_data_cols,
    allowed_networks = .cst$allowed_networks
  ) |>
    dplyr::distinct(monitor_type, .keep_all = TRUE)

  result <- meta$monitor_type |>
    purrr::map2(
      meta$site_id,
      ~ load_aqmap_plot_data(
        network = .x,
        site_id = .y,
        aqmap_url = .cst$aqmap_url,
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
