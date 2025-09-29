make_icon_svg <- function(
  groups,
  values,
  font_sizes = c(119, 99, 90),
  marker_size,
  marker_size_missing = NULL,
  for_legend = FALSE,
  force = FALSE,
  icon_dir = system.file("images/icons", package = "aqmapr"),
  template_dir = system.file("images", package = "aqmapr")
) {
  stopifnot(is.character(groups), length(groups) > 0, all(!is.na(groups)))
  stopifnot(is.numeric(values), length(values) == length(groups))
  stopifnot(
    is.numeric(font_sizes),
    length(font_sizes) == 3,
    all(!is.na(font_sizes))
  )
  stopifnot(is.numeric(marker_size), length(marker_size) == 1)
  stopifnot(
    is.null(marker_size_missing) |
      (is.numeric(marker_size_missing) & length(marker_size_missing) == 1)
  )
  stopifnot(is.logical(force), length(force) == 1)

  # Combine inputs
  icons <- data.frame(group = groups, values) |>
    # Build icon path
    dplyr::mutate(
      path = .data$group |>
        make_icon_path(
          values = .data$values,
          icon_dir = icon_dir,
          for_legend = for_legend
        )
    ) |>
    # Ignore any duplicates
    dplyr::filter(!duplicated(.data$path))

  # Build icon details
  icons <- icons |>
    dplyr::mutate(
      # Displayed value
      value = .data$values |>
        make_icon_text(for_legend = for_legend),
      n_chars = nchar(.data$value),
      # Fill & text colour
      fill = .data$values |>
        aqhi::AQHI_plus(detailed = FALSE) |>
        aqhi::get_aqhi_colours(),
      stroke = prismatic::best_contrast(.data$fill),
      # Size of icons/text
      font_size = font_sizes[
        (.data$n_chars %in% 1:3) |> ifelse(yes = .data$n_chars, no = 1)
      ] |>
        as.character(),
      size = is.na(values) |>
        ifelse(yes = marker_size_missing, no = marker_size) |>
        as.character()
    )

  # Ignore icons which already exists unless force == TRUE
  icons <- icons |>
    dplyr::filter(!file.exists(.data$path) | force)

  # Do nothing if no new icons needed
  if (nrow(icons) == 0) {
    return(invisible())
  }

  # Build icon svg from templates
  placeholder_cols <- c("value", "font_size", "size", "fill", "stroke")
  icons$svg <- template_dir |>
    icon_from_svg_template(
      groups = icons$group,
      placeholders = icons[, placeholder_cols]
    )

  # Write out icon files
  icons$path |>
    purrr::walk2(icons$svg, ~ writeLines(.y, .x)) |>
    invisible()
}

# Create icon path for each network/concentration pair
make_icon_path <- function(
  groups,
  values,
  icon_dir,
  for_legend = FALSE
) {
  stopifnot(is.character(groups), length(groups) > 0, all(!is.na(groups)))
  stopifnot(is.numeric(values), length(values) > 0)
  stopifnot(is.character(icon_dir), length(icon_dir) == 1)

  # Handle station marker icons
  icon_values <- values |>
    make_icon_text(for_legend = for_legend)

  # Build icon path
  icon_url_template <- file.path(icon_dir, "%s_icon_%s.svg")
  icon_url_template |> sprintf(groups, icon_values)
}

icon_from_svg_template <- function(dir, groups, placeholders) {
  stopifnot(is.character(dir), length(dir) == 1)
  stopifnot(is.character(groups), length(groups) > 0)
  stopifnot(is.data.frame(placeholders))

  # Check if icon svg templates exists
  template_paths <- dir |>
    file.path("%s_icon_template.svg") |>
    sprintf(unique(groups)) |>
    stats::setNames(unique(groups))
  stopifnot(all(file.exists(template_paths)))

  # Read in templates
  svg_templates <- template_paths |>
    lapply(\(x) readChar(x, nchars = file.info(x)$size))

  # Match up templates with input names
  svg <- unlist(svg_templates[groups])

  # Replace template placeholders with each icons details
  for (placeholder in names(placeholders)) {
    svg <- svg |>
      stringr::str_replace_all(
        pattern = paste0("\\{", placeholder, "\\}"),
        replacement = placeholders[[placeholder]]
      )
  }

  # Handle weird vertical centering of "+"
  is_plus <- placeholders$value == "+"
  svg[is_plus] <- svg[is_plus] |>
    gsub(
      pattern = 'dominant-baseline="central"',
      replacement = 'alignment-baseline="middle"'
    )
  return(svg)
}

make_icon_text <- function(
  icon_values,
  for_legend = FALSE,
  allowed_range = c(0, 999)
) {
  stopifnot(length(icon_values) > 0, is.numeric(icon_values))
  stopifnot(
    is.logical(for_legend),
    length(for_legend) == 1 | length(for_legend) == length(icon_values)
  )
  stopifnot(is.numeric(allowed_range), length(allowed_range) == 2)

  # Repeat for_legend if needed
  if (length(for_legend) == 1) {
    for_legend <- rep(for_legend, length(icon_values))
  }

  # Round values (no decimals), censor below min allowed
  rounded <- round(icon_values)
  rounded[rounded < allowed_range[1]] <- NA

  # Build icon text
  icon_text <- as.character(rounded)
  icon_text[is.na(rounded)] <- "-" # Missing values
  icon_text[rounded > allowed_range[2]] <- "+" # Values above limit
  icon_text[for_legend] <- "" # Don't show any text for legend icons

  return(icon_text)
}
