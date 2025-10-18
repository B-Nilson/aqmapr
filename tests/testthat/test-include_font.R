test_that("basic case works", {

  font_urls <- c(
    "Inter" = 'https://fonts.googleapis.com/css2?family=Inter:ital,opsz,wght@0,14..32,100..900;1,14..32,100..900&display=swap'
  )
  make_leaflet_map() |> 
    include_font(font_urls = font_urls) |>
    expect_no_error() |> 
    expect_no_warning() |> 
    expect_snapshot()
  
  make_leaflet_map() |> 
    include_font(font_urls = font_urls, force_display = TRUE) |>
    expect_no_error() |> 
    expect_no_warning() |> 
    expect_snapshot()
})
