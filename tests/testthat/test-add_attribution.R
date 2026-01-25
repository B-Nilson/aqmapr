test_that("basic case works", {
  map <- make_leaflet_map() |>
    add_attribution(
      "test attribution",
      "<a href='https://google.com'>test url</a>"
    ) |>
    expect_no_error() |> 
    expect_no_warning()
})