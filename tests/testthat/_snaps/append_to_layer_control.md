# no error occurs

    Code
      expect_no_warning(expect_no_error(append_to_layer_control(leaflet::addMarkers(
        leaflet::addMarkers(append_to_layer_control(leaflet::addMarkers(
          append_to_layer_control(leaflet::addProviderTiles(leaflet::addProviderTiles(
            append_to_layer_control(leaflet::addProviderTiles(leaflet::leaflet(),
            "OpenStreetMap", group = "test"), base_groups = "test"), "OpenStreetMap",
            group = "test2"), "OpenStreetMap", group = "test3"), base_groups = c(
            "test2", "test3")), lat = -37.8136, lng = 144.9631, group = "test4"),
        layer_groups = "test4"), lat = -35.8136, lng = 144.9631, group = "test5"),
        lat = -33.8136, lng = 144.9631, group = "test6"), layer_groups = c("test5",
        "test6"))))

