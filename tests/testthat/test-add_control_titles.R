test_that("basic case works", {
  map <- make_leaflet_map(
    point_layers = list(PointLayer(
      group = "test",
      data = data.frame(lat = 20, lng = 20, pm25 = 100) |>
        sf::st_as_sf(coords = c("lng", "lat")),
      fill_palette = eer_smoke_pal(),
      fill = ~pm25,
      popup = ~ as.character(pm25)
    ))
  )

  temp_file <- tempfile(fileext = ".html")

  w_js <- map |>
    add_control_titles(
      base_title = "--- Basemaps ---",
      layers_title = "--- Layers ---"
    )

  rlang::check_installed("webshot")
  w_js |> save_map(save_to = temp_file)
  on.exit({
    unlink(temp_file)
  })

  page <- temp_file |> readLines()
  body <- page[
    which(page == "<body>"):which(page == "</body>")
  ]

  expect_true(any(grepl(
    body,
    pattern = "add_control_titles('--- Basemaps ---', '--- Layers ---');",
    fixed = TRUE
  )))
})
