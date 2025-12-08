center_on_popup = function (map, open_popup, zoom) {
    if (zoom) map.setZoom(zoom);
    var px = map.project(open_popup._latlng);
    px.y -= open_popup._container.clientHeight / 2;
    map.panTo(map.unproject(px))
}
