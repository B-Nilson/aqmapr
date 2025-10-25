validator_len_1 <- function(value) {
  if (length(value) != 1) "must be length 1"
}

class_colour <- S7::new_union(
  S7::class_function,
  S7::class_formula,
  S7::class_character,
  S7::class_list
)

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
    layer_id = class_character,
    class_name = class_character,
    pane = class_character |>
      new_property(
        validator = \(value) {
          if (length(value) != 1) {
            "must be length 1"
          } else if (
            !value %in%
              c(
                "overlayPane",
                "shadowPane",
                "markerPane",
                "tooltipPane",
                "mapPane",
                "popupPane",
                "tilePane"
              )
          ) {
            "must be one of overlayPane, shadowPane, markerPane, tooltipPane, mapPane, popupPane, tilePane"
          }
        },
        default = "overlayPane"
      ),
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
    opacity = class_double |>
      new_property(
        default = 0.8,
        validator = validator_len_1
      ),
    transparent = class_logical |>
      new_property(
        default = FALSE,
        validator = validator_len_1,
        getter = \(self) {
          self@opacity < 1
        }
      ),
    interactive = class_logical |>
      new_property(
        default = TRUE,
        validator = validator_len_1
      ),
    bubbling_mouse_events = class_logical |>
      new_property(
        default = TRUE,
        validator = validator_len_1
      )
  )
)
