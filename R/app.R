source("functions.R")
app <- ambiorix::Ambiorix$new(port = 8000L, host = "127.0.0.1")

app$get("/", \(req, res) {
  map <- make_leaflet_map()
  res$htmlwidget(map)
})

app$start()