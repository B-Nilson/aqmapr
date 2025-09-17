
app <- ambiorix::Ambiorix$new(port = 8000L, host = "127.0.0.1")

app$get("/", \(req, res) {
  res$send("Hello, World!")
})

app$start()