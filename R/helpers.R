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
  # Group together low cost monitors if deisred
  if (group_lcms) {
    is_lcm <- names(.allowed_networks) != "agency"
    allowed_networks <- list(
      agency = .allowed_networks$agency,
      lcm = unlist(.allowed_networks[is_lcm])
    )
  } else {
    allowed_networks <- .allowed_networks
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
  if (all(text %in% names(.allowed_networks))) {
    text |>
      factor(
        levels = names(.allowed_networks),
        labels = unname(.text$networks[names(.allowed_networks)])
      ) |>
      as.character()
  } else {
    stop("This type of text is not supported")
  }
}
