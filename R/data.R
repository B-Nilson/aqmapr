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

#' Canadian provinces from OpenStreetMap
#'
#' Locations, names, and abbreviations of all provinces and territories in Canada from OpenStreetMap.
#' Created by calling [handyr::get_communities()] on all provinces and territories in Canada.
#'
#' @format ## `canadian_provinces`
#' An `sf` data frame with 13 rows and 4 columns:
#' \describe{
#'   \item{osm_id}{OpenStreetMap ID}
#'   \item{name}{Name of province or territory}
#'   \item{abbr}{Abbreviation of province or territory}
#'   \item{type}{Either "province" or "territory"}
#'   \item{geometry}{`sf` geometry column for province/territory boundary}
#'   ...
#' }
#' @source <https://openstreetmap.org>
"canadian_provinces"
