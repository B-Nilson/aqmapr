#' Canadian communities from OpenStreetMap
#'
#' Locations and names of all city, town, village, and hamlet communities in Canada from OpenStreetMap as of October 2025.
#' Created by calling [handyr::get_communities()] on all provinces and territories in Canada.
#'
#' @format ## `canada_communities`
#' An `sf` data frame with 11,313 rows and 5 columns:
#' \describe{
#'   \item{osm_id}{OpenStreetMap ID}
#'   \item{name}{Name of community}
#'   \item{prov_terr}{Province or territory of community}
#'   \item{type}{Type of community (city, town, village, hamlet)}
#'   \item{geometry}{`sf` geometry column for community point location}
#'   ...
#' }
#' @source <https://openstreetmap.org>
"canada_communities"