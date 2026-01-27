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
        if (any(lengths(value) != 1)) {
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
  needs_pull <- "formula" %in% class(out$values) & has_data
  needs_pull_has_pal <- needs_pull & !is.null(palette)
  if (is_list_form) {
    out <- value
    if (needs_pull) {
      out$values <- data |>
        dplyr::pull(!!rlang::as_quosure(out$values))
    }
    value <- out$palette(out$values)
  } else if (needs_pull_has_pal) {
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
