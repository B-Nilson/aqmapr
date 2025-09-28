test_that("no error occurs", {
  leaflet::leaflet() |> 
    track_map_state() |> 
    expect_no_error() |> 
    expect_no_warning()
})
