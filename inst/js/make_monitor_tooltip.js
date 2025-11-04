
const tooltip_template_dir = "/html"
const tooltip_template_paths = {
    tooltip: tooltip_template_dir +  "/monitor_tooltip.html",
    row: tooltip_template_dir +  "/monitor_popup_table_row.html"
};
let tooltip_templates = load_html_templates(tooltip_template_paths);

async function make_monitor_tooltip(
    station_name,
    monitor_type,
    values = { date_stamp: "2025-01-01T01:00:00Z", pm25_1hr: -1, pm25_3hr: -1, pm25_24hr: -1 }
) {
    let pm25_units = "&mu;g m <sup>-3</sup>";
    let placeholders = {
        "title": station_name,
        "subtitle": monitor_type + " monitor",
        "table_header": "As of: " + values.date_stamp,
        "values": {
            "10-min average": { value: values.pm25_10min ?? "-", units: pm25_units },
            "1-hour average": { value: values.pm25_1hr ?? "-", units: pm25_units },
            "3-hour average": { value: values.pm25_3hr ?? "-", units: pm25_units },
            "24-hour average": { value: values.pm25_24hr ?? "-", units: pm25_units }
        }
    }
    if (!("pm25_10min" in values)) {
        delete placeholders["values"]["10-min average"];
    }
    try {
        let popup = await make_popup(placeholders);
        return popup
    } catch (error) {
        console.error("Error making monitor popup:", error);
    }
}

async function make_tooltip(
    placeholders = {
        "title": "Title",
        "subtitle": "Subtitle",
        "table_header": "Table Header",
        "values": {
            "name1": { value: "1", units: "units" },
            "name2": { value: "1", units: "units" }
        }
    }
) {
    let templates = await tooltip_templates;
    // Replace placeholders in the templates
    let tooltip_rows = Object.keys(placeholders.values).map((name) => {
        let value = placeholders.values[name];
        return replace_placeholders(templates.row, { "header": name, "value": value.value, "units": value.units });
    });

    let tooltip = replace_placeholders(
        templates.tooltip,
        Object.assign(placeholders, { "table_rows": tooltip_rows.join("") })
    );
    return tooltip
};
