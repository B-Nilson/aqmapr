#' Start the Ambiorix server
#'
#' Start the Ambiorix server and listen on the specified port and host.
#'
#' @export
start_server <- function() {
  .cst <- load_constants()
  app <- ambiorix::Ambiorix$new(
    port = .cst$server$port,
    host = .cst$server$host
  )

  app$static(.cst$icon_dir$local, .cst$icon_dir$server)

  # i.e. /data/recent/json
  app$get("/data/:name/:type", get_data)

  # i.e. /data/plotting/agency/10102/json
  app$get("/data/:name/:network/:site_id/:type", get_data)

  app$get("/", get_map)

  app$start()
}
