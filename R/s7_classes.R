validator_len_1 <- function(value) {
  if (length(value) != 1) "must be length 1"
}

validator_len_0_1 <- function(value) {
  if (length(value) > 1) "must be length 0 or 1"
}

class_colour <- S7::new_union(
  S7::class_function,
  S7::class_formula,
  S7::class_character,
  S7::class_list
)

class_flag_on <- class_logical |>
  new_property(
    default = TRUE,
    validator = validator_len_1
  )

class_flag_off <- class_logical |>
  new_property(
    default = FALSE,
    validator = validator_len_1
  )

class_leaflet_pane <- class_character |>
  new_union(S7::class_list) |>
  new_property(
    validator = \(value) {
      allowed <- c(
        "overlayPane",
        "shadowPane",
        "markerPane",
        "tooltipPane",
        "mapPane",
        "popupPane",
        "tilePane"
      )
      if (is.list(value)) {
        if (!identical(sort(names(value)), c("name", "zindex"))) {
          "list must have names 'name' and 'zindex'"
        }
        if (any(sapply(value, length) != 1)) {
          "list values must be length 1"
        }
        if (!is.character(value$name)) {
          "name must be character"
        }
        if (value$name %in% allowed) {
          "name must not be one of" |>
            paste(paste(allowed, collapse = ", "))
        }
        if (!is.numeric(value$zindex)) {
          "zindex must be numeric"
        }
      } else {
        if (length(value) != 1) {
          "must be length 1"
        } else if (!value %in% allowed) {
          "must be one of" |>
            paste(paste(allowed, collapse = ", "))
        }
      }
    },
    default = "overlayPane"
  )

class_leaflet_position <- class_character |>
  new_property(
    default = "bottomleft",
    validator = \(value) {
      allowed <- c("topleft", "topright", "bottomleft", "bottomright")
      if (!value %in% allowed) {
        "must be one of" |>
          paste(paste(allowed, collapse = ", "))
      } else if (length(value) != 1) {
        "must be length 1"
      }
    }
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
      new_property(validator = validator_len_0_1),
    layer_id = class_character,
    class_name = class_character,
    pane = class_leaflet_pane,
    legend_position = class_leaflet_position,
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
    interactive = class_flag_on,
    bubbling_mouse_events = class_flag_on
  )
)

# Define generic method to add layers to leaflet map
add_to_map <- S7::new_generic("add_to_map", "layer")
