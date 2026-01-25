function on_locator_click(btn, map) {
    let zoom = 15;
    map.locate({ setView: false });
    map.once('locationfound', (e) => map.setView(e.latlng, zoom));
};
