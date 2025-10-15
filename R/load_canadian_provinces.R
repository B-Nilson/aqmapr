#' Canadian provinces from OpenStreetMap
#'
#' Polygons and names of provinces and territories in Canada from OpenStreetMap.
#' Created using [osmdata::opq()] and [osmdata::osmdata_sf()].
#'
#' @return
#' An `sf` data frame with 13 rows and 4 columns:
#' \describe{
#'   \item{osm_id}{OpenStreetMap ID}
#'   \item{name, abbr}{Name/abbreviation of province or territory}
#'   \item{type}{Type of entry (province or territory)}
#'   \item{geometry}{`sf` geometry column for community point location}
#' }
#' @source <https://openstreetmap.org>
#' @export
load_canadian_provinces <- function() {
  "extdata/canadian_provinces.rds" |>
    system.file(package = "aqmapr") |>
    readRDS() |> 
    withr::with_package(package = "sf")
}
