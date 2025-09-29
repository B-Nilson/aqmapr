test_that("make_icon_path() works", {
  .cst <- load_constants()
  networks <- names(.cst$allowed_networks)
  pm25_1hr <- c(50, NA, 1000, rep(1, length(networks) - 3))
  for_legend <- c(TRUE, rep(FALSE, length(pm25_1hr) - 1))
  result <- make_icon_path(
    groups = networks,
    values = pm25_1hr,
    icon_dir = .cst$icon_dir$local,
    for_legend = for_legend
  )

  # check length
  length(result) |>
    expect_equal(length(.cst$allowed_networks))
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
  .cst <- load_constants()
  temp_dir <- tempdir()
  networks <- names(.cst$allowed_networks)
  result <- networks |>
    make_icon_svg(
      values = c(10, NA, 1, 1000),
      for_legend = c(TRUE, FALSE, FALSE, FALSE),
      icon_dir = temp_dir,
      font_sizes = .cst$font_sizes$markers,
      marker_size = .cst$marker_sizes$obs,
      marker_size_missing = .cst$marker_sizes$missing,
      force = TRUE
    )
  files_created <- temp_dir |>
    list.files(pattern = "*_icon_[-,1,+]?.svg", full.names = TRUE)

  length(files_created) |>
    expect_equal(length(.cst$allowed_networks))

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
