test_that("no error occurs", {
  leaflet::leaflet() |>
    leaflet::addProviderTiles("OpenStreetMap", group = "test") |>
    append_to_layer_control(base_groups = "test") |>
    leaflet::addProviderTiles("OpenStreetMap", group = "test2") |>
    leaflet::addProviderTiles("OpenStreetMap", group = "test3") |>
    append_to_layer_control(base_groups = c("test2", "test3")) |>
    leaflet::addMarkers(lat = -37.8136, lng = 144.9631, group = "test4") |>
    append_to_layer_control(layer_groups = "test4") |>
    leaflet::addMarkers(lat = -35.8136, lng = 144.9631, group = "test5") |>
    leaflet::addMarkers(lat = -33.8136, lng = 144.9631, group = "test6") |>
    append_to_layer_control(layer_groups = c("test5", "test6")) |>
    expect_no_error() |>
    expect_no_warning() |>
    expect_snapshot()
})
