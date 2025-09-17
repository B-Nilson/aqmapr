source("functions.R")
source("helpers.R")
source("./handlers/get_data.R")
source("./handlers/get_map.R")

app <- ambiorix::Ambiorix$new(port = 8000L, host = "127.0.0.1")

# i.e. /data/recent/json
app$get("/data/:name/:type", get_data)

# i.e. /data/plotting/agency/10102/json
app$get("/data/:name/:network/:site_id/:type", get_data)

app$get("/", get_map)

app$start()
