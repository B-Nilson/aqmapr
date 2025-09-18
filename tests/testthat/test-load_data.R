test_that("load_recent_aqmap_data() works", {
  .cst <- load_constants()
  data_dir <- tempdir()
  result <- load_recent_aqmap_data(data_dir = data_dir)
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
  networks <- c("agency", "purpleair", "aqegg")
  # TODO: what if these go offline permanently?
  site_ids <- c(10102, 182, "egg00805f7bd1a80143")

  result <- networks |> 
    purrr::map2(site_ids, ~ load_aqmap_plot_data(network = .x, site_id = .y)) |> 
    stats::setNames(networks) 
 
  # Returned data is correct
  for(network in networks) {
    expect_s3_class(result[[network]], "data.table")
    expect_true(nrow(result[[network]]) > 0)
    expect_true("pm25" %in% names(result[[network]]))
  }
})
