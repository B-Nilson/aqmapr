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

color_property <- function() {
  class_colour |>
    S7::new_property(
      default = quote(colour),
      getter = function(self) {
        self@colour
      },
      setter = function(self, value) {
        if (identical(value, self@colour)) {
          return(self)
        }
        self@colour <- value
        self
      }
    )
}

colour_setter <- function(self, value) {
  parsed <- value |>
    parse_colours(
      data = self@data,
      palette = self@colour_palette
    )
  print(parsed)

  self@colour_palette <- parsed$palette
  self@colour_values <- parsed$values
  self@colour <- parsed$colours
  return(self)
}

fill_setter <- function(self, value) {
  parsed <- value |>
    parse_colours(
      data = self@data,
      palette = self@fill_palette
    )
  print(parsed)
  self@fill_palette <- parsed$palette
  self@fill_values <- parsed$values
  self@fill <- parsed$colours
  return(self)
}

parse_colours <- function(value, data = NULL, palette = NULL) {
  out <- list()
  is_list_form <- is.list(value) &
    identical(sort(names(value)), c("palette", "values"))
  has_data <- !is.null(data) & sum(dim(data)) > 0
  if (is_list_form) {
    out <- value
    if ("formula" %in% class(out$values) & has_data) {
      out$values <- data |>
        dplyr::pull(!!rlang::as_quosure(out$values))
    }
    value <- out$palette(out$values)
  } else if ("formula" %in% class(value) & has_data & !is.null(palette)) {
    out$values <- data |>
      dplyr::pull(!!rlang::as_quosure(value))
    out$palette <- palette
    value <- palette(out$values)
  } else {
    out$palette <- palette
    out$values <- value
  }
  out$colours <- value
  return(out)
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
