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
  icons <- data.frame(network = as.character(networks), pm25_1hr) |>
    # Build icon details
    dplyr::mutate(
      text = make_safe_icon_text(pm25_1hr),
      path = make_marker_icon_path(network, pm25_1hr),
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
    sprintf(unique(icons$network)) |>
    setNames(unique(icons$network)) |>
    lapply(
      \(x) {
        readLines(x) |>
          paste(collapse = "\n")
      }
    )
  icons$template <- svg_templates[icons$network] |> 
    unlist()

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
