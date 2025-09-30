LeafletWidget.methods.addJsonPointerLayer = function (json_url, layer_id, group, options) {
    fetch(json_url)
        .then(response => response.json())
        .then(data => {
            const layer = L.geoJSON(data, options);
            if (layer_id || group) {
                _map.layerManager.addLayer(layer, "geojson", layer_id, group);
            }
            layer.addTo(_map);
        });
};
