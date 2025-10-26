test_that("basic case with points data works", {
  point_layers <- list(PointLayer(
    group = "test",
    data = data.frame(lat = 20, lng = 20, pm25 = 100),
    fill_palette = eer_smoke_pal(),
    fill = ~pm25
  ))
  make_leaflet_map(point_layers = point_layers) |>
    expect_no_error() |>
    expect_no_warning() |>
    expect_snapshot()
})

test_that("basic case with polygons data works", {
  canadian_provinces <- load_canadian_provinces()
  make_leaflet_map(
    polygon_data = list("Provinces" = canadian_provinces),
    polygon_options = list(
      weight = 1,
      color = "black",
      fillColor = "black",
      fillOpacity = 0.1,
      opacity = 1,
      label = ~name
    )
  ) |>
    expect_no_error() |>
    expect_no_warning() |>
    expect_snapshot()
})

test_that("advanced case with points data works", {
  colour_pal <- leaflet::colorFactor(
    "viridis",
    domain = levels(canada_communities$type),
    ordered = TRUE,
    reverse = TRUE
  )
  point_layers <- canada_communities |>
    PointLayer(
      group = "Communities",
      data = _,
      fill_palette = colour_pal,
      fill = ~type,
      label = ~ paste("Name: ", name, "<br/>", "Type: ", type) |>
        lapply(htmltools::HTML)
    ) |> 
    list()

  make_leaflet_map(point_layers = point_layers) |>
    expect_no_error() |>
    expect_no_warning() |>
    expect_snapshot()
})

test_that("include_timestamp works", {
  make_leaflet_map(include_timestamp = TRUE) |>
    expect_no_error() |>
    expect_no_warning()

  make_leaflet_map(
    include_timestamp = as.POSIXct("2022-01-01 00:00:00", tz = "UTC")
  ) |>
    expect_no_error() |>
    expect_no_warning() |>
    expect_snapshot()
})
