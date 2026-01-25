test_that("basic case works", {
  map <- leaflet::leaflet() |>
    add_locator_button() |>
    expect_no_error() |> 
    expect_no_warning()
})
