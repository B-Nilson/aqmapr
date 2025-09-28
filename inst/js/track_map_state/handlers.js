// TODO: wait_for(location.hash).then(...)
wait_for_hash().then(async () => {
    _map.args =  get_args();
    // Hide all layers
    let all_layer_idxs = Object.keys(_map.layers.data).map(Number);
    hide_layers(all_layer_idxs);
    // Display URL layers and set view (center/zoom)
    apply_args(set_view = true);
    // Make sure the URL arguments match displayed
    update_url_args();
}).then(() => {
    _map.on('overlayadd', handle_layer_change);
    _map.on('overlayremove', handle_layer_change);
    _map.on('baselayerchange', handle_layer_change);
    _map.on('moveend', handle_map_move);
});

async function handle_layer_change(el) {
    // Update _map.args object with layer change
    if (el.type === 'overlayadd') {
        // Add layer id if layer just added
        _map.args.data.push("L" + el.layer._leaflet_id);
    } else if (el.type === 'overlayremove') {
        // Remove layer id if layer just removed
        _map.args.data.splice(
            _map.args.data.indexOf("L" + el.layer._leaflet_id), 1
        );
    } else if (el.type === 'baselayerchange') {
        // Update base layer id
        _map.args.base = "B" + el.layer._leaflet_id;
    }
    // Update URL to reflect _map.args
    update_url_args();
}

async function handle_map_move() {
    // Update URL to reflect _map.args
    update_url_args();
}
