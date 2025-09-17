# Determine when a local file was last updated relative to when it should have been
# TODO: support basic file index urls
# TODO: move to handyr
get_file_age <- function(local_path, since = Sys.time()) {
  if (!file.exists(local_path)) {
    return(as.difftime(Inf, units = "days"))
  }
  last_update_time <- file.info(local_path)$mtime

  since - last_update_time
}

# Standardize varieties of network inputs
translate_network <- function(networks, group_lcms = TRUE, as_factor = FALSE) {
  allowed_networks <- list(
    agency = c("fem", "naps", "agency", "fems"),
    lcm = c("lcm", "lcms"),
    purpleair = c("pa", "purpleair", "pas", "purpleairs"),
    aqegg = c("aqegg", "egg", "eggs")
  )

  # Group together low cost monitors if deisred
  if (group_lcms) {
    is_lcm <- names(allowed_networks) != "agency"
    allowed_networks <- list(
      agency = allowed_networks$agency,
      lcm = unlist(allowed_networks[is_lcm])
    )
  }

  # Include a placeholder if singular so max.col behave as expected
  remove_last <- length(networks) == 1
  if (remove_last) {
    networks <- c(networks, networks)
  }

  # Determine which network
  which_network <- allowed_networks |>
    sapply(\(x) tolower(networks) %in% x) |>
    max.col()

  # Handle edge cases
  if (remove_last) {
    which_network <- which_network[-length(which_network)]
  }
  if (any(is.na(which_network))) {
    stop(paste0("Unknown network: ", networks[is.na(which_network)]))
  }

  # Return standardized network name (factorize if needed)
  standardized_network <- names(allowed_networks)[which_network]
  if (as_factor) {
    standardized_network <- standardized_network |>
      factor(levels = names(allowed_networks))
  }
  return(standardized_network)
}

# Translate from code names to pretty names for display
pretty_text <- function(text) {
  if (all(text %in% c("agency", "lcm", "purpleair", "aqegg"))) {
    text |>
      factor(
        levels = c("agency", "lcm", "purpleair", "aqegg"),
        labels = c("Regulatory", "Low-cost", "PurpleAir", "AQegg")
      ) |>
      as.character()
  } else {
    stop("This type of text is not supported")
  }
}

# Create icon path for each network/concentration pair
make_marker_icon_path <- function(networks, pm25_1hr) {
  icon_url_template <- "../images/icons/%s_icon_%s.svg"
  icon_shapes <- list(agency = 23, lcm = 21, purpleair = 21, aqegg = 22)

  # Get shape for each network
  network_shapes <- unlist(icon_shapes[networks])

  # Calculate AQHI+ for each concentration
  aqhi_plus <- pm25_1hr |>
    aqhi::AQHI_plus() |>
    dplyr::mutate(
      level = level |>
        as.character() |> # de-factor
        handyr::swap(NA, "-") # handle missing values
    )

  # Format concentrations for text
  pm25_1hr_safe <- aqhi_plus$pm25_1hr_ugm3 |>
    make_safe_icon_text()

  # Build icon path, and attach colour
  icon_url_template |>
    sprintf(networks, pm25_1hr_safe) |>
    setNames(aqhi_plus$colour)
}

make_icon_svg <- function(networks, pm25_1hr, force = FALSE) {
  # Combine inputs
  icons <- data.frame(networks, pm25_1hr) |>
    # Build icon details
    dplyr::mutate(
      text = make_safe_icon_text(pm25_1hr),
      path = make_marker_icon_path(networks, pm25_1hr),
      fill_colour = names(path) |> handyr::swap(NA, with = "#bbbbbb"),
      text_colour = prismatic::best_contrast(fill_colour),
      font_size = dplyr::case_when(
        pm25_1hr <= 9 ~ 117,
        pm25_1hr <= 99 ~ 99,
        pm25_1hr <= 999 ~ 73,
        TRUE ~ 130 # shown as "+"
      ) |>
        as.character(),
      size = ifelse(is.na(pm25_1hr), 17, 33) |>
        as.character()
    ) |>
    # Ignore if icon already exists or is duplicated
    dplyr::filter(!file.exists(path) & !duplicated(path) | force)

  # Do nothing if no new icons needed
  if (nrow(icons) == 0) {
    return(invisible())
  }

  # Attach icon templates
  svg_templates <- "../images/%s_icon_template.svg" |>
    sprintf(unique(networks)) |>
    setNames(unique(networks)) |>
    lapply(
      \(x) {
        readLines(x) |>
          paste(collapse = "\n")
      }
    )
  icons$template <- svg_templates[icons$networks]

  # Replace placeholders
  icons$svg <- icons$template |>
    stringr::str_replace_all("\\{value\\}", icons$text) |>
    stringr::str_replace_all("\\{font-size\\}", icons$font_size) |>
    stringr::str_replace_all("\\{size\\}", icons$size) |>
    stringr::str_replace_all("\\{fill\\}", icons$fill_colour) |>
    stringr::str_replace_all("\\{stroke\\}", icons$text_colour)

  # Handle weird vertical centering of "+"
  icons$svg[icons$text == "+"] <- icons$svg[icons$text == "+"] |>
    stringr::str_replace_all(
      'dominant-baseline="central"',
      'alignment-baseline="middle"'
    )

  # Write out icons
  icons$path |>
    purrr::walk2(icons$svg, ~ writeLines(.y, .x))
}

make_safe_icon_text <- function(icon_values) {
  icon_values |>
    round() |>
    handyr::clamp(range = c(0, 1000)) |>
    handyr::swap(1000, with = "+") |>
    handyr::swap(NA, with = "-")
}
