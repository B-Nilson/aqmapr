test_that("non-referenced works", {
  leaflet::leaflet() |>
    include_scripts(
      paths = "./inst/js/on_render.js",
      as_reference = FALSE
    ) |>
    include_scripts(
      texts = "const x = 1;",
      types = "js",
      as_reference = FALSE
    )
})
