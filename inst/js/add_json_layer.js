LeafletWidget.methods.addJsonPointerLayer = async function (
    json_url, layer_id, group, 
    options = {}, _add_to_map = false,
    tooltip_options = {
        permanent: false,
        direction: "right",
        offset: [Math.round(iconSize / 2), 0]
    },
    popup_options = {
        offset: [0, -5],
        minWidth: 330,
        closeOnClick: false
    },
    keys = { iconUrl: "iconUrl", pane: "pane", zIndexOffset: "zIndexOffset", iconSize: "iconSize", label: "label", popup: "popup" }
) {
    fetch(json_url)
        .then(response => response.json())
        .then(data => {
            const layer = L.geoJSON(data, {
                ...options,
                pointToLayer: function (feature, latlng) {
                    let data = feature.properties;
                    const iconUrl = data[keys.iconUrl];
                    const pane = data[keys.pane] ?? "markerPane";
                    const zIndexOffset = data[keys.zIndexOffset] ?? 0;
                    if (iconUrl) {
                        const iconSize = data[keys.iconSize] ?? 32;
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
                onEachFeature: async function (feature, layer) {
                    let data = feature.properties;
                    if (data && data[keys.label]) {
                        const iconSize = data[keys.iconSize] ?? 32;
                        layer.bindTooltip(data[keys.label], tooltip_options);
                    };

                    if (data && (data[keys.popup] || keys.popup.startsWith("JS:::"))) {
                        if (keys.popup.startsWith("JS:::")) {
                            layer.bindPopup(await eval(keys.popup.substring(5)), popup_options);
                        } else {
                            layer.bindPopup(data[keys.popup], popup_options);
                        };
                    };
                }

            });
            if (layer_id || group) {
                _map.layerManager.addLayer(layer, "geojson", layer_id, group);
            }
            if (_add_to_map) layer.addTo(_map);
        });
};
