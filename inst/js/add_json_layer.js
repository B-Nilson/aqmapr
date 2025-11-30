LeafletWidget.methods.addJsonPointerLayer = async function (
    json_url, layer_id, group,
    // see https://leafletjs.com/reference.html#geojson
    options = {
        style: function (feature) { return {}; },
        filter: function (feature) { return true; },
        // coordsToLatLng: L.coordsToLatLng,
        markersInheritOptions: false,
        interactive: true,
        bubblingMouseEvents: true,
        pane: "overlayPane",
        attribution: null
    },
    _add_to_map = false,
    // see https://leafletjs.com/reference.html#tooltip
    tooltip_options = {
        pane: "popupPane",
        offset: [5, 0],
        direction: "right",
        permanent: false,
        sticky: false,
        opacity: 1,
        interactive: false,
        className: '',
        content: '',
        bubblingMouseEvents: true,
        attribution: null
    },
    // see https://leafletjs.com/reference.html#popup
    popup_options = {
        pane: "popupPane",
        offset: [0, -5],
        minWidth: 330,
        maxWidth: 330,
        maxHeight: null,
        autoPan: true,
        autoPanPadding: [5, 5],
        keepInView: false,
        closeButton: true,
        autoClose: true,
        closeOnEscapeKey: true,
        closeOnClick: true,
        className: '',
        interactive: true,
        content: '',
        bubblingMouseEvents: true,
        attribution: null
    },
    // TODO: cleanup defaults
    keys = { iconUrl: "iconUrl", keyboard: "keyboard", pane: "pane", zIndexOffset: "zIndexOffset", iconSize: "iconSize", label: "label", popup: "popup" }
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
                // add tooltips/popups if available
                onEachFeature: async function (feature, layer) {
                    let data = feature.properties;
                    // add tooltips
                    if (data && (data[keys.label] || keys.label.startsWith("JS:::"))) {
                        let tooltip = keys.label.startsWith("JS:::") ?
                            await eval(keys.label.substring(5)) :
                            data[keys.label];
                        layer.bindTooltip(tooltip, tooltip_options);
                    };
                    // add popups
                    if (data && (data[keys.popup] || keys.popup.startsWith("JS:::"))) {
                        let popup = keys.popup.startsWith("JS:::") ?
                            await eval(keys.popup.substring(5)) :
                            data[keys.popup];
                        layer.bindPopup(popup, popup_options);
                    };
                }

            });
            if (layer_id || group) {
                _map.layerManager.addLayer(layer, "geojson", layer_id, group);
            }
            if (_add_to_map) layer.addTo(_map);
        });
};
