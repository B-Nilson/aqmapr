# basic case with points data works

    Code
      expect_no_warning(expect_no_error(make_leaflet_map(point_layers = point_layers)))

# basic case with polygons data works

    Code
      expect_no_warning(expect_no_error(make_leaflet_map(polygon_data = list(
        Provinces = canadian_provinces), polygon_options = list(weight = 1, color = "black",
        fillColor = "black", fillOpacity = 0.1, opacity = 1, label = ~name))))

# advanced case with points data works

    Code
      expect_no_warning(expect_no_error(make_leaflet_map(point_layers = point_layers)))

# include_timestamp works

    Code
      expect_no_warning(expect_no_error(make_leaflet_map(include_timestamp = as.POSIXct(
        "2022-01-01 00:00:00", tz = "UTC"))))

