make_leaflet_map <- function() {
  leaflet::leaflet() |> 
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap)
}