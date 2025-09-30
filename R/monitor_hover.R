make_monitor_hover = function(
  name,
  network,
  monitor_type,
  date_last_obs,
  pm25_10min,
  pm25_1hr,
  pm25_3hr,
  pm25_24hr,
  text = list(
    type = "Type: ",
    monitor = "Monitor: ",
    time = "Time: ",
    pm_title = "PM<sub>2.5</sub> averages:",
    pm_10min = "10 min.:",
    pm_1hr = "1 hr.:",
    pm_3hr = "3 hr.:",
    pm_24hr = "24 hr.:",
    no_data = "No Data."
  )
) {
  stopifnot(is.character(name), length(name) > 0, all(!is.na(name)))
  stopifnot(is.factor(network), length(network) > 0, all(!is.na(network)))
  stopifnot(lubridate::is.POSIXct(date_last_obs), length(date_last_obs) > 0)
  stopifnot(is.list(text), length(text) > 0, c("type", "time") %in% names(text))

  # Build hover text
  paste0(
    paste0("<big><b>", name |> stringr::str_sub(end = 20), "</b></big><br>"),
    paste0(text$type, "<b>", pretty_text(network), "</b><br>"),
    paste0(text$monitor, "<b>", pretty_text(monitor_type), "</b><br>"),
    paste0(text$time, "<b>", pretty_text(date_last_obs), "</b><br>"),
    make_pm_summary_table(
      network = network,
      pm25_10min = pm25_10min,
      pm25_1hr = pm25_1hr,
      pm25_3hr = pm25_3hr,
      pm25_24hr = pm25_24hr,
      text = text
    )
  )
}

make_pm_summary_table <- function(
  network,
  pm25_10min,
  pm25_1hr,
  pm25_3hr,
  pm25_24hr,
  text = list(
    type = "Type: ",
    time = "Time: ",
    pm_title = "PM<sub>2.5</sub> averages:",
    pm_10min = "10 min.:",
    pm_1hr = "1 hr.:",
    pm_3hr = "3 hr.:",
    pm_24hr = "24 hr.:",
    no_data = "No Data."
  )
) {
  stopifnot(is.factor(network), length(network) > 0, all(!is.na(network)))
  stopifnot(is.list(text), length(text) > 0, "pm_title" %in% names(text))
  stopifnot(
    length(network) == length(pm25_10min),
    length(network) == length(pm25_1hr),
    length(network) == length(pm25_3hr),
    length(network) == length(pm25_24hr)
  )
  pm25_units <- "&mu;g m <sup>-3</sup>"
  # Hide 10 min. average for agency
  recent_row <- text$pm_10min |>
    make_obs_table_row(
      value = pm25_10min,
      units = pm25_units,
      missing_text = text$no_data
    )
  recent_row[network == "agency"] <- ""

  # Build table
  paste0(
    "<table>",
    "<tr><th>",
    text$pm_title,
    "</th></tr>",
    recent_row,
    text$pm_1hr |>
      make_obs_table_row(
        value = pm25_1hr,
        units = pm25_units,
        missing_text = text$no_data
      ),
    text$pm_3hr |>
      make_obs_table_row(
        value = pm25_3hr,
        units = pm25_units,
        missing_text = text$no_data
      ),
    text$pm_24hr |>
      make_obs_table_row(
        value = pm25_24hr,
        units = pm25_units,
        missing_text = text$no_data
      ),
    "</table>"
  )
}

make_obs_table_row <- function(label, value, units, missing_text = "No Data.") {
  stopifnot(is.character(label), length(label) > 0)
  stopifnot(is.character(value) | is.numeric(value), length(value) > 0)
  stopifnot(is.character(units), length(units) > 0)
  stopifnot(is.character(missing_text), length(missing_text) > 0)
  stopifnot(length(label) == 1 | length(label) == length(value))
  stopifnot(length(units) == 1 | length(units) == length(value))
  stopifnot(length(missing_text) == 1)

  value <- value |>
    handyr::swap(NA, with = missing_text)
  paste0(
    "<tr><td style='width: 1%;'>",
    label,
    "</td><td><b>",
    value,
    "</b> ",
    units,
    "</td></tr>"
  )
}
