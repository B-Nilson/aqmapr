test_that("make_icon_path() works", {
  networks <- c("agency", "lcm", "purpleair", "aqegg")
  pm25_1hr <- c(50, NA, 1000, rep(1, length(networks) - 3))
  for_legend <- c(TRUE, rep(FALSE, length(pm25_1hr) - 1))
  result <- make_icon_path(
    groups = networks,
    values = pm25_1hr,
    icon_dir = system.file("images/icons", package = "aqmapr"),
    for_legend = for_legend
  )

  # check length
  length(result) |>
    expect_equal(length(networks))
  # check right name (order of pieces etc)
  result |>
    basename() |>
    expect_equal(
      networks |>
        paste0(
          "_icon_",
          pm25_1hr |> make_icon_text(for_legend = for_legend)
        ) |>
        paste0(".svg")
    )
})

test_that("make_icon_svg() works", {
  temp_dir <- tempdir()
  networks <- c("agency", "lcm", "purpleair", "aqegg")
  result <- networks |>
    make_icon_svg(
      values = c(10, NA, 1, 1000),
      for_legend = c(TRUE, FALSE, FALSE, FALSE),
      icon_dir = temp_dir,
      marker_size = 33,
      marker_size_missing = 19,
      force = TRUE
    )
  files_created <- temp_dir |>
    list.files(pattern = "*_icon_[-,1,+]?.svg", full.names = TRUE)

  length(files_created) |>
    expect_equal(length(networks))

  file.remove(files_created)
})

test_that("make_icon_text() works", {
  expect_equal(make_icon_text(1), "1")
  expect_equal(
    make_icon_text(
      c(1000, 50, 50.1, NA),
      for_legend = c(FALSE, TRUE, FALSE, FALSE)
    ),
    c("+", "", "50", "-")
  )
})
