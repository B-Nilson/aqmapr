test_that("basic case works", {
  page_title <- "test"
  map <- make_leaflet_map()

  temp_file <- tempfile(fileext = ".html")

  w_js <- map |>
    center_on_opened_popup()

  w_js |>
    htmlwidgets::saveWidget(file = temp_file, selfcontained = FALSE)
  on.exit({
    unlink(temp_file)
  })

  page <- temp_file |> readLines()
  header <- page[
    which(page == "<head>"):which(page == "</head>")
  ]

  lines_with_center_on_popup <- header |>
    stringr::str_subset(pattern = "center_on_popup")
  expect_true(length(lines_with_center_on_popup) >= 1)
})
