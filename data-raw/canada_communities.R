prov_terrs <- c(
  "British Columbia",
  "Alberta",
  "Saskatchewan",
  "Manitoba",
  "Ontario",
  "Quebec",
  "New Brunswick",
  "Nova Scotia",
  "Prince Edward Island",
  "Newfoundland and Labrador",
  "Yukon",
  "Northwest Territories",
  "Nunavut"
) |>
  paste0(", Canada")

canada_communities <- prov_terrs |>
  handyr::for_each(
    handyr::get_communities,
    .name = TRUE,
    .bind_id = "prov_terr"
  ) |>
  dplyr::select("osm_id", "name", "prov_terr", "type") |>
  dplyr::mutate(
    prov_terr = prov_terr |>
      factor(
        levels = prov_terrs,
        labels = prov_terrs |> stringr::str_remove(", Canada")
      ),
    type = factor(type, c("city", "town", "village", "hamlet"))
  )

row.names(canada_communities) <- NULL

usethis::use_data(canada_communities, overwrite = TRUE)
