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
translate_network <- function(network) {
  allowed_networks <- list(
    agency = c("fem", "naps", "agency", "fems"),
    purpleair = c("pa", "purpleair", "pas", "purpleairs"),
    aqegg = c("aqegg", "egg", "eggs")
  )
  network <- allowed_networks |>
    sapply(\(x) tolower(network) %in% x)
  if (!any(network)) {
    stop("Network not supported")
  }
  names(network)[which(network)]
}