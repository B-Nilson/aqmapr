async function wait_for_hash() {
    return new Promise((resolve) => {
        const interval = setInterval(() => {
            if (typeof _map !== "undefined" && location.hash) {
                clearInterval(interval);
                resolve();
            }
        }, 10);
    });
}

// Build URL arguments from leaflet ids
function get_args() {
    // Parse URL hash into base/data/location
    let url_args = get_url_args();

    // Assign defaults where needed and return dict
    const default_args = get_default_args();
    return {
        "base": url_args.base ? url_args.base : default_args.base,
        "data": url_args.data.length ? url_args.data : default_args.data,
        "location": url_args.location
    };
}

function get_url_args() {
    // Init
    let base_layer = null;
    let data_layers = [];
    let loc_args = [];

    // Parse URL hash into base/data/location
    let url_args = location.hash.split('/');
    for (arg of url_args) {
        if (arg.includes('B')) {
            base_layer = arg;
        } else if (arg.includes('L')) {
            data_layers.push(arg);
        } else {
            loc_args.push(arg);
        }
    }
    return {
        "base": base_layer,
        "data": data_layers,
        "location": loc_args
    }
}

function get_default_args() {
    // Get leaflet id for default base layer and preprend "B"
    let default_base = "B" + _map.layers.base[_layers.base.indexOf(_default_layers.base)].layer._leaflet_id;
    // Get leaflet ids for default data layers and preprend "L"
    let default_layers = _map.layers.defaults.map(layer => "L" + layer._leaflet_id);
    // Combine and return
    return {
        "base": default_base,
        "data": default_layers,
        "location": []
    }
}

// Set map view, base layer, and data layers from args
function apply_args(set_view = true) {
    // Set zoom/center if desired
    if (set_view && _map.args.location.length === 3) {
        set_map_view(..._map.args.location);
    }
    // Set base layer
    let base_layer_idx = _map.layers.base.map(
        (layer) => "B" + (layer.layer._leaflet_id + 1)).indexOf(_map.args.base);
    set_base_layer(base_layer_idx);
    // Set data layers
    let data_layer_idxs = _map.args.data.map(
        (id) => _map.layers.data.map(
            (layer) => "L" + layer._leaflet_id).indexOf(id));
    show_layers(data_layer_idxs);
}

// Update args in URL to match current state
function update_url_args() {
    // Remove Bxx and Lxx from hash that aren't in args
    for (url_arg of location.hash.split('/')) {
        if (url_arg.includes("L") || url_arg.includes("B")) {
            if (!(_map.args.data.includes(url_arg) | _map.args.base == url_arg)) {
                location.hash = location.hash.replace("/" + url_arg, "");
            };
        };
    }

    // Append base layer arg if not present
    if (!location.hash.includes(_map.args.base)) {
        location.hash = location.hash + "/" + _map.args.base;
    };

    // Append data layer args if not present
    for (arg of _map.args.data) {
        if (!location.hash.includes(arg)) {
            location.hash = location.hash + "/" + arg;
        };
    }
}