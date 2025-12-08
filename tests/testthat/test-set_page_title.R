test_that("basic case works", {
  page_title <- "test"
  map <- make_leaflet_map()

  temp_file <- tempfile()

  w_title <- map |>
    set_page_title(page_title = page_title)

  w_title |> save_map(save_to = temp_file)
  on.exit({
    unlink(temp_file)
  })

  page <- temp_file |> readLines()
  header <- page[
    which(page == "<head>"):which(page == "</head>")
  ] |>
    paste(collapse = "")

  header |>
    stringr::str_extract_all(pattern = "<title>.+?</title>") |>
    unlist() |>
    dplyr::last() |>
    expect_equal("<title>%s</title>" |> sprintf(page_title))
})
