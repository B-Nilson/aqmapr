# basic case with points data works

    Code
      expect_no_warning(expect_no_error(leaflet::addLegend(make_leaflet_map(
        point_data = list(Communities = canada_communities), point_options = list(
          radius = 3, weight = 1, color = "black", fillColor = ~ colour_pal(type),
          fillOpacity = 0.8, opacity = 1, label = ~ lapply(paste("Name: ", name,
            "<br/>", "Type: ", type), htmltools::HTML))), pal = colour_pal, values = sort(
        unique(canada_communities$type)))))

# basic case with polygons data works

    Code
      expect_no_warning(expect_no_error(make_leaflet_map(polygon_data = list(
        Provinces = canadian_provinces), polygon_options = list(weight = 1, color = "black",
        fillColor = "black", fillOpacity = 0.1, opacity = 1, label = ~name))))

# advanced case with points data works

    Code
      expect_no_warning(expect_no_error(leaflet::addLegend(make_leaflet_map(
        point_data = split(canada_communities, canada_communities$type),
        point_options = list(radius = 3, weight = 1, color = "black", fillColor = ~
          colour_pal(type), fillOpacity = 0.8, opacity = 1, label = ~ lapply(paste(
            "Name: ", name, "<br/>", "Type: ", type), htmltools::HTML))), pal = colour_pal,
      values = sort(unique(canada_communities$type)))))

