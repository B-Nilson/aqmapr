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
    display_by_default = class_flag_on,
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

#' S7 generic to add a layer to a Leaflet map
#'
#' Add a layer to a map using [add_to_map()].
#'
#' @param layer A leaflet layer to add to the map. Created using [PointLayer()], [PolygonLayer()], or [WMSLayer()].
#' @param map A leaflet map to add the layer to.
#' @param ... Additional arguments to pass to the layer's `add_to_map` method.
#' @return A leaflet map with the layer added
#' @export
add_to_map <- "add_to_map" |>
  S7::new_generic(dispatch_args = "layer", fun = function(layer, map, ...) {
    # Create custom panes as needed
    if (is.list(layer@pane)) {
      map <- map |>
        leaflet::addMapPane(
          name = layer@pane$name,
          zIndex = layer@pane$zindex
        )
    }

    if (!"aqmapr::WMSLayer" %in% class(layer)) {
      pane_name <- is.list(layer@pane) |>
        ifelse(layer@pane$name, layer@pane)
      # Add referenced geojson if url provided
      if (length(layer@data_url)) {
        map <- map |>
          add_geojson_layer(
            layer_id = layer@layer_id,
            json_url = layer@data_url,
            options = c(
              list(pane = pane_name),
              layer@options
            ),
            group = layer@group,
            add_to_layer_control = FALSE,
            as_reference = TRUE
          )
      }
      # Add legend if desired
      if (
        layer@use_fill &
          length(layer@fill_values) &
          length(layer@group) &
          !identical(layer@fill_values, layer@fill)
      ) {
        map <- map |>
          leaflet::addLegend(
            data = layer@data,
            group = layer@group,
            pal = layer@fill_palette,
            values = layer@fill_values,
            opacity = layer@opacity,
            position = layer@legend_position,
            title = layer@group
          )
      }
    }

    # Add add to layer control if desired
    if (length(layer@group)) {
      map <- map |>
        append_to_layer_control(
          layer_groups = layer@group
        )
    }

    # Hide layer if desired
    if (!layer@display_by_default) {
      map <- map |>
        leaflet::hideGroup(group = layer@group)
    }

    S7::S7_dispatch()
  })
