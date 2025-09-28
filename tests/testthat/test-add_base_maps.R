test_that("no error occurs", {
  base_maps <- c(
    "Light Theme" = leaflet::providers$OpenStreetMap,
    "Terrain" = leaflet::providers$Stamen.Terrain
  )

  leaflet::leaflet() |>
    add_base_maps(base_maps) |>
    expect_no_error() |>
    expect_no_warning()
})
