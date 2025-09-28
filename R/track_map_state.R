track_map_state <- function(map) {
  map |> 
    leaflet.extras::addHash()
}