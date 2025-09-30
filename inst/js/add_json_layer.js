LeafletWidget.methods.addJsonPointerLayer = function (json_url, layer_id, options) {
    fetch(json_url)
        .then(response => response.json())
        .then(data => {
            const layer = L.geoJSON(data, options);
            if (layer_id) {
                _map.layerManager.addLayer(layer_id, layer);
            }
            layer.addTo(_map);
        });
};
