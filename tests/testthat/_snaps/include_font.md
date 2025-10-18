# basic case works

    Code
      expect_no_warning(expect_no_error(include_font(make_leaflet_map(), font_urls = font_urls)))

---

    Code
      expect_no_warning(expect_no_error(include_font(make_leaflet_map(), font_urls = font_urls,
      force_display = TRUE)))

