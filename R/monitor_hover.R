make_monitor_hover = function(
  name,
  network,
  date_last_obs,
  pm25_10min,
  pm25_1hr,
  pm25_3hr,
  pm25_24hr,
  pm25_units,
  text
) {
  # Format date for converting to local using js later
  date_last_obs <- date_last_obs |>
    format("%Y-%m-%dT%H:%M:%SZ")

  # Build hover text
  paste0(
    paste0("<big><b>", name |> stringr::str_sub(end = 20), "</b></big><br>"),
    paste0(text$type, "<b>", pretty_text(network), "</b><br>"),
    paste0(text$time, "<b>", date_last_obs, "</b><br>"),
    make_pm_summary_table(
      network = network,
      pm25_10min = pm25_10min,
      pm25_1hr = pm25_1hr,
      pm25_3hr = pm25_3hr,
      pm25_24hr = pm25_24hr,
      pm25_units = pm25_units,
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
  pm25_units,
  text
) {
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
