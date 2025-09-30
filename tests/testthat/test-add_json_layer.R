test_that("no error occurs", {
  geojson_path <- "https://gist.githubusercontent.com/wavded/1200773/raw/e122cf709898c09758aecfef349964a8d73a83f3/sample.json"

  leaflet::leaflet() |>
    add_json_layer(json_url = geojson_path) |>
    expect_no_error() |>
    expect_no_warning()
})