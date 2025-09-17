source("functions.R")
source("get_data.R")
app <- ambiorix::Ambiorix$new(port = 8000L, host = "127.0.0.1")

app$get("/data/:name/:type", get_data)

app$get("/", get_map)

app$start()