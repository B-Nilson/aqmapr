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
