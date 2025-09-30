test_that("non-referenced works", {
  leaflet::leaflet() |>
    include_scripts(
      paths = system.file("js/on_render.js", package = "aqmapr"),
      as_reference = FALSE
    ) |>
    include_scripts(
      texts = "const x = 1;",
      types = "js",
      as_reference = FALSE
    )
})
