load_constants = function() {
  list(
    server = list(
      host = "127.0.0.1",
      port = 8000
    ),

    data_dir = tempdir(),

    image_dir = system.file("images", package = "aqmapr"),
    js_dir = system.file("js", package = "aqmapr"),

    icon_dir = list(
      local = system.file("images/icons", package = "aqmapr"),
      server = "/icons"
    ),
    force_update_icons = FALSE,

    allowed_networks = list(
      agency = c("fem", "naps", "agency", "fems"),
      lcm = c("lcm", "lcms"),
      purpleair = c("pa", "purpleair", "pas", "purpleairs"),
      aqegg = c("aqegg", "egg", "eggs")
    ),

    recent_data_cols = c(
      site_id = "sensor_index",
      name = "monitor",
      "network",
      "monitor_type", # (un-grouped network - i.e PA or EGG instead of lcm)
      "lat",
      "lng",
      "prov_terr",
      date_last_obs = "date",
      pm25_10min = "pm25_recent",
      "pm25_1hr",
      "pm25_3hr",
      "pm25_24hr"
    ),

    base_maps = c(
      "Light Theme" = "OpenStreetMap",
      "Dark Theme" = "CartoDB.DarkMatter"
    ),

    font_sizes = list(
      # 1, 2, and 3 digit values on observation markers
      markers = c(119, 99, 90)
    ),

    marker_sizes = list(missing = 17, obs = 33, legend = 22), # px

    aqmap_url = "https://aqmap.ca/aqmap",

    units = list(
      pm25 = "&mu;g m <sup>-3</sup>"
    ),

    text = list(
      networks = list(
        agency = "Regulatory",
        lcm = "Low-cost",
        purpleair = "PurpleAir",
        aqegg = "AQegg"
      ),
      monitor_hover = list(
        type = "Type: ",
        time = "Time: ",
        pm_title = "PM<sub>2.5</sub> averages:",
        pm_10min = "10 min.:",
        pm_1hr = "1 hr.:",
        pm_3hr = "3 hr.:",
        pm_24hr = "24 hr.:",
        no_data = "No Data."
      ),
      monitor_legend = list(
        title = htmltools::HTML("PM<sub>2.5</sub> Monitors"),
        hover = paste(
          "Fine particulate matter monitor types.",
          "Values are in units of &mu;g m<sup>-3</sup>,",
          "and symbols are coloured using the Canadian AQHI+ system."
        )
      )
    )
  )
}
