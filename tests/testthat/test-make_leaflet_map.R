test_that("basic case with points data works", {
  colour_pal <- leaflet::colorFactor(
    "viridis",
    domain = levels(canada_communities$type),
    ordered = TRUE,
    reverse = TRUE
  )
  make_leaflet_map(
    point_data = list("Communities" = canada_communities),
    point_options = list(
      radius = 3,
      weight = 1,
      color = "black",
      fillColor = ~ type,
      fillOpacity = 0.8,
      palette = colour_pal,
      opacity = 1,
      label = ~ paste("Name: ", name, "<br/>", "Type: ", type) |>
        lapply(htmltools::HTML)
    )
  ) |>
    leaflet::addLegend(
      pal = colour_pal,
      values = unique(canada_communities$type) |> sort()
    ) |>
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
  make_leaflet_map(
    point_data = canada_communities |> split(canada_communities$type),
    point_options = list(
      radius = 3,
      weight = 1,
      color = "black",
      fillColor = ~ colour_pal(type),
      fillOpacity = 0.8,
      opacity = 1,
      label = ~ paste("Name: ", name, "<br/>", "Type: ", type) |>
        lapply(htmltools::HTML)
    )
  ) |>
    leaflet::addLegend(
      pal = colour_pal,
      values = unique(canada_communities$type) |> sort()
    ) |>
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
