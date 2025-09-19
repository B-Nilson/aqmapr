# Create icon path for each network/concentration pair
make_marker_icon_path <- function(networks, pm25_1hr, icon_dir) {
  stopifnot(is.character(networks), length(networks) > 0, all(!is.na(networks)))
  stopifnot(is.numeric(pm25_1hr), length(pm25_1hr) > 0)
  stopifnot(is.character(icon_dir), length(icon_dir) == 1)

  icon_url_template <- file.path(icon_dir, "%s_icon_%s.svg")
  icon_shapes <- list(agency = 23, lcm = 21, purpleair = 21, aqegg = 22)

  # Get shape for each network
  network_shapes <- unlist(icon_shapes[networks])

  # Handle station marker icons
  is_station_marker <- pm25_1hr == -1
  pm25_1hr[is_station_marker] <- pm25_1hr[!is_station_marker] |>
    mean(na.rm = TRUE)

  # Calculate AQHI+ for each concentration
  aqhi_plus <- aqhi::AQHI_plus(pm25_1hr)

  # Handle station marker icons
  aqhi_plus$pm25_1hr_ugm3[is_station_marker] <- -1

  # Build icon path, and attach colour
  icon_url_template |>
    sprintf(networks, make_safe_icon_text(aqhi_plus$pm25_1hr_ugm3)) |>
    stats::setNames(aqhi_plus$colour |> handyr::swap(NA, with = "#bbbbbb"))
}

make_icon_svg <- function(
  networks,
  pm25_1hr,
  template_dir,
  icon_dir,
  font_sizes,
  marker_sizes,
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

  # Combine inputs
  icons <- data.frame(network = networks, pm25_1hr) |>
    # Build icon details
    dplyr::mutate(
      text = make_safe_icon_text(.data$pm25_1hr),
      path = make_marker_icon_path(
        .data$network,
        .data$pm25_1hr,
        icon_dir = icon_dir
      ),
      fill_colour = names(.data$path),
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

make_safe_icon_text <- function(icon_values) {
  stopifnot(length(icon_values) > 0, is.numeric(icon_values))
  icon_values |>
    round() |>
    handyr::clamp(range = c(-1, 1000)) |>
    handyr::swap(1000, with = "+") |>
    handyr::swap(-1, with = "") |>
    handyr::swap(NA, with = "-")
}
