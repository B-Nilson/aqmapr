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

# Create icon url for AQmap monitor marker
make_aqmap_marker_icon_url <- function(networks, pm25_1hr) {
  icon_url_template <- "https://aqmap.ca/aqmap/icons/icon_%s_%s.png"
  icon_shapes <- list(agency = 23, lcm = 21, purpleair = 21, aqegg = 22)

  # Get shape for each network
  network_shapes <- unlist(icon_shapes[networks])

  # Calculate AQHI+ for each concentration
  aqhi_plus <- pm25_1hr |>
    aqhi::AQHI_plus() |>
    dplyr::pull(AQHI_plus) |>
    as.character() |>
    handyr::swap(NA, "-") # handle missing values

  # Build icon url
  icon_url_template |>
    sprintf(network_shapes, aqhi_plus)
}
