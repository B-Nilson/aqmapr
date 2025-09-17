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
translate_network <- function(networks) {
  allowed_networks <- list(
    agency = c("fem", "naps", "agency", "fems"),
    purpleair = c("pa", "purpleair", "pas", "purpleairs"),
    aqegg = c("aqegg", "egg", "eggs")
  )
  which_network <- allowed_networks |>
    sapply(\(x) tolower(networks) %in% x) |> 
    max.col()
  if (any(is.na(which_network))) {
    stop(paste0("Unknown network: ", networks[is.na(which_network)]))
  }
  names(allowed_networks)[which_network]
}

# Translate from code names to pretty names for display
pretty_text <- function(text) {
  if (all(text %in% c("agency", "purpleair", "aqegg"))) {
    text |> 
      factor(
        levels = c("agency", "purpleair", "aqegg"),
        labels = c("Agency", "PurpleAir", "AQegg")
      ) |> 
      as.character()
  }else {
    stop("This type of text is not supported")
  }

}
