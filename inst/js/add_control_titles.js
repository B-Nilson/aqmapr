function add_control_titles(base_title, layers_title) {
    if (base_title) add_base_control_title(base_title);
    if (layers_title) add_layer_control_title(layers_title);
}

function add_layer_control_title(title) {
    let className = 'leaflet-control-layers-overlays';
    add_control_title(title, className);
}

function add_base_control_title(title) {
    let className = 'leaflet-control-layers-base';
    add_control_title(title, className);
}

function add_control_title(title, className) {
    let element = document.getElementsByClassName(className)[0];
    if (title && element) element.prepend(title);
}
