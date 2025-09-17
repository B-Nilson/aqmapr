
app <- ambiorix::Ambiorix$new(port = 8000L)

app$get("/", \(req, res) {
  res$send("Hello, World!")
})

app$start()