
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
    let default_layers = [];
    for (let i in _map.layerManager._groupContainers) {
        let layer = _map.layerManager._groupContainers[i];
        // Split out base, data, and default layers
        if (layer.groupname != undefined) {
            let is_base = _layers.base.includes(layer.groupname);
            let is_data = _layers.data.includes(layer.groupname);
            let is_default = _default_layers.data.includes(layer.groupname) || _default_layers.base == layer.groupname;
            if (is_base) base_layers.push(layer);
            if (is_data) data_layers.push(layer);
            if (is_default) default_layers.push(layer);
        }
    };
    return {
        base: get_base_layers(_layers.base),
        data: data_layers,
        defaults: default_layers
    };
}

function get_base_layers(base_layer_names) {
    var base_layers = [];
    for (let [key, layer] of Object.entries(_map.layerManager._byStamp)) {
        if (base_layer_names.includes(layer.group)) {
            base_layers.push(layer);
            if (base_layers.length == base_layer_names.length) break
        } else continue;
    }
    return base_layers;
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