
// TODO: set this via R
var _layers = {
    "base": ["Light Theme", "Dark Theme"],
    "data": ["Regulatory", "Low-cost"]
};

// TODO: set this via R
var _default_layers = {
    "data": [0, 1], // same order as data_layers
    "base": 0 // same order as base_layers
}

function get_layers() {
    // Get layer names for default layers
    let initial_layers = [];
    _default_layers.data.forEach(idx => initial_layers.push(_layers.data[idx]));
    let base = _layers.base[_default_layers.base];

    // Loop through map layers and get pointers 
    let base_layers = [];
    let data_layers = [];
    let default_layers = [];
    for (let i in _map._layers) {
        let layer = _map._layers[i];
        // Push relevent data layers to .all
        if (layer.groupname != undefined &
            layer.groupname != 'search' &
            layer.groupname != base) data_layers.push(layer);
        // Push base layers to .base
        if (layer.groupname == base) base_layers.push(layer);
        // Push default layers to .defaults
        if (initial_layers.includes(layer.groupname)) default_layers.push(layer);
        // stop running if we got all the layers we need
        if (data_layers.length === _layers.data.length &
            base_layers.length === _layers.base.length &
            default_layers.length === initial_layers.length) {
            break
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