test_that("no error occurs", {
  leaflet::leaflet() |>
    add_base_maps(base_maps = "OpenStreetMap") |>
    track_map_state() |>
    expect_no_error() |>
    expect_no_warning()
})
