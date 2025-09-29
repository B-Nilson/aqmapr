
// TODO: set this via R
var _layers = {
    "base": ["Light Theme", "Dark Theme"],
    "data": ["Regulatory", "Low-cost"]
};

// TODO: set this via R
var _default_layers = {
    "data": ["Regulatory", "Low-cost"],
    "base": "Light Theme"
}

function get_layers() {
    // Loop through map layers and get pointers 
    let base_layers = [];
    let data_layers = [];
    let defaults = {
        "base": null,
        "data": []
    };
    let base_i = 0;
    let layer_i = 0;
    for (let key in _map.layerManager._groupContainers) {
        let layer = _map.layerManager._groupContainers[key];
        // Split out base, data, and default layers
        if (layer.groupname != undefined) {
            let is_base = _layers.base.includes(layer.groupname);
            let is_data = _layers.data.includes(layer.groupname);
            let is_default = _default_layers.data.includes(layer.groupname) || _default_layers.base == layer.groupname;
            if (is_base) {
                // Add basemap id for tracking
                layer._basemap_id = base_i++;
                base_layers.push(layer);
                if (is_default) defaults.base = layer;
            }
            if (is_data) {
                // Add layer id for tracking
                layer._layer_id = layer_i++;
                data_layers.push(layer);
                if (is_default) defaults.data.push(layer);
            }
        }

    };
    return {
        base: base_layers,
        data: data_layers,
        defaults: defaults
    };
}

function show_layers(layer_idxs) {
    layer_idxs.forEach(
        (idx) => {
            const layer = _map.layers.data[idx];
            if (layer) _map.addLayer(layer);
        }
    );
}

function hide_layers(layer_idxs) {
    layer_idxs.forEach(
        (idx) => {
            const layer = _map.layers.data[idx];
            if (layer) _map.removeLayer(layer);
        }
    );
}

function set_base_layer(layer_idx) {
    for (var x = 0; x < _layers.base.length; x++) {
        e = _map.currentLayersControl._form[x];
        if (e.name != "leaflet-base-layers" || e == null) continue;
        if (x == layer_idx) {
            e.click()
        }
    }
}

function set_map_view(zoom, lat, lng) {
    _map.setView(
        L.latLng(lat, lng),
        zoom.replace('#', '')
    );
}