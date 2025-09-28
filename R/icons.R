# Create icon path for each network/concentration pair
make_marker_icon_path <- function(
  networks,
  pm25_1hr,
  icon_dir,
  for_legend = FALSE
) {
  stopifnot(is.character(networks), length(networks) > 0, all(!is.na(networks)))
  stopifnot(is.numeric(pm25_1hr), length(pm25_1hr) > 0)
  stopifnot(is.character(icon_dir), length(icon_dir) == 1)

  # Handle station marker icons
  icon_values <- pm25_1hr |>
    make_safe_icon_text(for_legend = for_legend)

  # Build icon path
  icon_url_template <- file.path(icon_dir, "%s_icon_%s.svg")
  icon_url_template |> sprintf(networks, icon_values)
}

make_icon_svg <- function(
  networks,
  pm25_1hr,
  template_dir,
  icon_dir,
  font_sizes,
  marker_sizes,
  for_legend = FALSE,
  force = FALSE
) {
  stopifnot(is.character(networks), length(networks) > 0, all(!is.na(networks)))
  stopifnot(is.numeric(pm25_1hr), length(pm25_1hr) > 0)
  stopifnot(is.character(template_dir), length(template_dir) == 1)
  stopifnot(
    is.numeric(font_sizes),
    length(font_sizes) > 0,
    all(!is.na(font_sizes))
  )
  stopifnot(
    is.list(marker_sizes),
    is.numeric(unlist(marker_sizes)),
    length(marker_sizes) > 0,
    all(!is.na(marker_sizes))
  )
  stopifnot(is.logical(force), length(force) == 1)

  # Combine inputs
  icons <- data.frame(network = networks, pm25_1hr) |>
    # Build icon details
    dplyr::mutate(
      text = .data$pm25_1hr |>
        make_safe_icon_text(for_legend = for_legend),
      path = .data$network |>
        make_marker_icon_path(
          pm25_1hr = .data$pm25_1hr,
          icon_dir = icon_dir,
          for_legend = for_legend
        ),
      fill_colour = .data$pm25_1hr |>
        aqhi::AQHI_plus(detailed = FALSE) |>
        aqhi::get_aqhi_colours(),
      text_colour = prismatic::best_contrast(.data$fill_colour),
      font_size = dplyr::case_when(
        .data$pm25_1hr <= 9 | is.na(.data$pm25_1hr) | .data$pm25_1hr > 999 ~
          font_sizes[1],
        .data$pm25_1hr <= 99 ~ font_sizes[2],
        .data$pm25_1hr <= 999 ~ font_sizes[3]
      ) |>
        as.character(),
      size = is.na(pm25_1hr) |>
        ifelse(marker_sizes$missing, marker_sizes$obs) |>
        as.character()
    ) |>
    # Ignore if icon already exists or is duplicated
    dplyr::filter(!file.exists(.data$path) & !duplicated(.data$path) | force)

  # Do nothing if no new icons needed
  if (nrow(icons) == 0) {
    return(invisible())
  }

  # Attach icon templates
  svg_templates <- template_dir |>
    file.path("%s_icon_template.svg") |>
    sprintf(unique(icons$network)) |>
    stats::setNames(unique(icons$network)) |>
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

make_safe_icon_text <- function(icon_values, for_legend = FALSE) {
  stopifnot(length(icon_values) > 0, is.numeric(icon_values))
  stopifnot(
    is.logical(for_legend),
    length(for_legend) == 1 | length(for_legend) == length(icon_values)
  )

  # Repeat for_legend if needed
  if (length(for_legend) == 1) {
    for_legend <- rep(for_legend, length(icon_values))
  }

  safe_text <- icon_values |>
    round() |>
    handyr::clamp(range = c(0, 1000)) |>
    handyr::swap(1000, with = "+") |>
    handyr::swap(NA, with = "-")
  safe_text[for_legend] <- ""
  
  return(safe_text)
}
