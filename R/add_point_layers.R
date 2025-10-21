
validator_len_1 <- function(value) {
  if (length(value) != 1) "must be length 1"
}

#' Leaflet generic layer S7 class
#'
#' Represent a leaflet layer to be added to the map. Usually expanded by other classes then added to a map using [add_to_map()].
#'
#' @export
#' @import S7
LeafletLayer <- new_class(
  "LeafletLayer",
  properties = list(
    group = class_character |>
      new_property(validator = validator_len_1),
    legend_position = class_character |>
      new_property(
        default = "bottomleft",
        validator = \(value) {
          if (
            !value %in%
              c("topleft", "topright", "bottomleft", "bottomright")
          ) {
            "must be one of topleft, topright, bottomleft, bottomright"
          } else if (length(value) != 1) {
            "must be length 1"
          }
        }
      ),
    transparent = class_logical |>
      new_property(
        default = FALSE,
        validator = validator_len_1,
        getter = \(self) {
          self@opacity < 1
        }
      ),
    opacity = class_double |>
      new_property(
        default = 0.8,
        validator = validator_len_1
      )
  )
)
