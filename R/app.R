source("functions.R")
app <- ambiorix::Ambiorix$new(port = 8000L, host = "127.0.0.1")

app$get("/", \(req, res) {
  recent_aqmap_data <- load_recent_aqmap_data() |> 
    handyr::on_error(.return = NULL)
  map <- make_leaflet_map(marker_data = recent_aqmap_data)
  res$htmlwidget(map)
})

app$start()