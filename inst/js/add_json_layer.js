LeafletWidget.methods.addJsonPointerLayer = function (
    json_url, layer_id, group, options, _add_to_map = false,
    keys = { iconUrl: "iconUrl", pane: "pane", zIndexOffset: "zIndexOffset", iconSize: "iconSize", label: "label", popup: "popup" }
) {
    fetch(json_url)
        .then(response => response.json())
        .then(data => {
            const layer = L.geoJSON(data, {
                ...options,
                pointToLayer: function (feature, latlng) {
                    const iconUrl = feature.properties[keys.iconUrl];
                    const pane = feature.properties[keys.pane] ?? "markerPane";
                    const zIndexOffset = feature.properties[keys.zIndexOffset] ?? 0;
                    if (iconUrl) {
                        const iconSize = feature.properties[keys.iconSize] ?? 32;
                        const icon = L.icon({
                            iconUrl: iconUrl,
                            iconSize: [iconSize, iconSize]
                        });
                        return L.marker(latlng, { icon: icon, pane: pane, zIndexOffset: zIndexOffset });
                    } else {
                        // Use default Leaflet marker
                        return L.marker(latlng, { pane: pane, zIndexOffset: zIndexOffset });
                    }
                },
                // Optional: add custom tooltip using .label property
                onEachFeature: function (feature, layer) {
                    if (feature.properties && feature.properties[keys.label]) {
                        const iconSize = feature.properties[keys.iconSize] ?? 32;
                        layer.bindTooltip(feature.properties[keys.label], {
                            permanent: false,
                            direction: "right",
                            offset: [Math.round(iconSize / 2), 0]
                        });
                    }

                    if (feature.properties && feature.properties[keys.popup]) {
                        layer.bindPopup(feature.properties[keys.popup]);
                    }
                }

            });
            if (layer_id || group) {
                _map.layerManager.addLayer(layer, "geojson", layer_id, group);
            }
            if (_add_to_map) layer.addTo(_map);
        });
};
