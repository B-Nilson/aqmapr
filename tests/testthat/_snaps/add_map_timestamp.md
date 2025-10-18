# basic case works

    Code
      expect_no_warning(expect_no_error(add_map_timestamp(make_leaflet_map(),
      timestamp = as.POSIXct("2022-01-01 00:00:00", tz = "UTC"))))

# en_francais works

    Code
      expect_no_warning(expect_no_error(add_map_timestamp(make_leaflet_map(),
      timestamp = as.POSIXct("2022-01-01 00:00:00", tz = "UTC"), en_francais = TRUE)))

