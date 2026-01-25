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
                    // See https://leafletjs.com/reference.html#marker
                    let opt = {
                        keyboard: data[keys.keyboard] ?? false,
                        title: data[keys.title] ?? "",
                        alt: data[keys.alt] ?? "Marker",
                        zIndexOffset: data[keys.zIndexOffset] ?? 0,
                        opacity: data[keys.opacity] ?? 1,
                        riseOnHover: data[keys.riseOnHover] ?? false,
                        riseOffset: data[keys.riseOffset] ?? 250,
                        pane: data[keys.pane] ?? "markerPane",
                        shadowPane: data[keys.shadowPane] ?? "shadowPane",
                        bubblingMouseEvents: data[keys.bubblingMouseEvents] ?? false,
                        autoPanOnFocus: data[keys.autoPanOnFocus] ?? true,
                        draggable: data[keys.draggable] ?? false,
                        autoPan: data[keys.autoPan] ?? true,
                        // autoPanPadding: data[keys.autoPanPadding] ?? L.point(50, 50),
                        autoPanSpeed: data[keys.autoPanSpeed] ?? 10,
                        interactive: data[keys.interactive] ?? true,
                        attribution: data[keys.attribution] ?? null
                    };
                    if (data[keys.iconUrl]) {
                        opt.icon = L.icon({
                            iconUrl: data[keys.iconUrl],
                            iconSize: [data[keys.iconSize] ?? 32, data[keys.iconSize] ?? 32],
                        });
                    }
                    return L.marker(latlng, opt);
                },
                // add tooltips/popups if available
                onEachFeature: async function (feature, layer) {
                    let data = feature.properties;
                    let has_tooltip = data && (data[keys.label] || keys.label.startsWith("JS:::"));
                    let has_popup = data && (data[keys.popup] || keys.popup.startsWith("JS:::"));
                    // add tooltips
                    if (has_tooltip) {
                        let tooltip = keys.label.startsWith("JS:::") ?
                            await eval(keys.label.substring(5)) :
                            data[keys.label];
                        layer.bindTooltip(tooltip, tooltip_options);
                    };
                    // add popups
                    if (has_popup) {
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
