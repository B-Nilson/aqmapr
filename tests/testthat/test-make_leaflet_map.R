test_that("basic case with points data works", {
  colour_pal <- leaflet::colorFactor("viridis", domain = canada_communities$type, ordered = TRUE)
  make_leaflet_map(
    point_data = list("Communities" = canada_communities),
    point_options = list(
      radius = 3,
      weight = 1,
      color = "black",
      fillColor = ~ leaflet::colorFactor("viridis", domain = name)(name),
      fillOpacity = 0.8,
      opacity = 1,
      label = ~ paste("Name: ", name, "<br/>", "Type: ", type) |>
        lapply(htmltools::HTML)
    )
  ) |>
    leaflet::addLegend(
      pal = colour_pal,
      values = levels(canada_communities$type) |>
        factor(levels = levels(canada_communities$type))
    ) |> 
      expect_no_error() |> 
      expect_no_warning() |> 
      expect_snapshot()
})
