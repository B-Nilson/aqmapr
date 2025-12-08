test_that("basic case works", {
  page_title <- "test"
  map <- make_leaflet_map(point_layers = list(PointLayer(
    group = "test",
    data = data.frame(lat = 20, lng = 20, pm25 = 100) |>
      sf::st_as_sf(coords = c("lng", "lat")),
    fill_palette = eer_smoke_pal(),
    fill = ~pm25,
    popup = ~as.character(pm25)
  )))

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
