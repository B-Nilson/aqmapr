test_that(".html works", {
  temp_file <- tempfile(fileext = ".html")
  on.exit({ unlink(temp_file) })

  map <- make_leaflet_map()
  save_map(map, save_to = temp_file)
  expect_true(file.exists(temp_file))
})

test_that(".png works", {
  temp_file <- tempfile(fileext = ".png")
  on.exit({ unlink(temp_file) })

  map <- make_leaflet_map()
  save_map(map, save_to = temp_file)
  expect_true(file.exists(temp_file))
})

test_that(".jpeg works", {
  temp_file <- tempfile(fileext = ".jpeg")
  on.exit({ unlink(temp_file) })

  map <- make_leaflet_map()
  save_map(map, save_to = temp_file)
  expect_true(file.exists(temp_file))
})

test_that(".pdf works", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit({ unlink(temp_file) })

  map <- make_leaflet_map()
  save_map(map, save_to = temp_file)
  expect_true(file.exists(temp_file))
})

# TODO: test encoding
# TODO: test page_title