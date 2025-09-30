test_that("no error occurs", {
  geojson_path <- "https://raw.githubusercontent.com/B-Nilson/aqmapr/refs/heads/main/inst/extdata/canadian_provinces.geojson"

  leaflet::leaflet() |>
    add_base_maps(base_maps = "OpenStreetMap") |>
    add_geojson_layer(json_url = geojson_path) |>
    expect_no_error() |>
    expect_no_warning()

  leaflet::leaflet() |>
    add_base_maps(base_maps = "OpenStreetMap") |>
    add_geojson_layer(json_url = geojson_path, as_reference = FALSE) |>
    expect_no_error() |>
    expect_no_warning()
})