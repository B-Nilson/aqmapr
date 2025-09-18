start_server <- function() {
  app <- ambiorix::Ambiorix$new(port = .server$port, host = .server$host)

  app$static(.icon_dir$local, .icon_dir$server)

  # i.e. /data/recent/json
  app$get("/data/:name/:type", get_data)

  # i.e. /data/plotting/agency/10102/json
  app$get("/data/:name/:network/:site_id/:type", get_data)

  app$get("/", get_map)

  app$start()
}
