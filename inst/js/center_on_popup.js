center_on_popup = function (map, zoom) {
    if (zoom) map.setZoom(zoom);
    var px = map.project(map._popup._latlng);
    px.y -= map._popup._container.clientHeight / 2;
    map.panTo(map.unproject(px))
}
