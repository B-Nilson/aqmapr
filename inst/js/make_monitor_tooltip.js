
const tooltip_template_dir = "/html"
const tooltip_template_paths = {
    template: tooltip_template_dir + "/monitor_tooltip.html",
    row: tooltip_template_dir + "/monitor_popup_table_row.html"
};
let tooltip_templates = load_html_templates(tooltip_template_paths);

async function make_monitor_tooltip(
    placeholders = { station_name: "Station Name", monitor_type: "Monitor Type", date_stamp: "2025-01-01T01:00:00Z", health_message: "Health message." },
    table_values = { pm25_10min: -1, pm25_1hr: -1 },
    table_labels = { pm25_10min: "10-min average", pm25_1hr: "1-hour average", pm25_3hr: "3-hour average", pm25_24hr: "24-hour average" },
    table_units = { pm25_10min: "&mu;g m <sup>-3</sup>", pm25_1hr: "&mu;g m <sup>-3</sup>", pm25_3hr: "&mu;g m <sup>-3</sup>", pm25_24hr: "&mu;g m <sup>-3</sup>" },
    title = "{{station_name}}",
    subtitle = "{{monitor_type}} monitor",
    table_header = "Observed PM<sub>2.5</sub> as of: {{date_stamp}}",
    footnote = "{{health_message}}",
    null_text = "-"
) {
    // Swap out any placeholders in the text variables
    title = replace_placeholders(title, placeholders);
    table_header = replace_placeholders(table_header, placeholders);
    subtitle = replace_placeholders(subtitle, placeholders);
    footnote = replace_placeholders(footnote, placeholders);

    // Build placeholders object for inserting text into templates
    let tooltip_placeholders = {
        "title": title,
        "subtitle": subtitle,
        "table_header": table_header,
        "footnote": footnote,
        "values": {}
    };
    for (let key in table_values) {
        tooltip_placeholders.values[key] = {
            value: table_values[key] ?? null_text,
            units: table_units[key],
            header: table_labels[key]
        };
    }

    // Build tooltip from html templates in html/
    try {
        let tooltip = await make_tooltip(tooltip_placeholders);
        return tooltip;
    } catch (error) {
        console.error("Error making monitor tooltip:", error);
    }
}

async function make_tooltip(
    placeholders = {
        "title": "Title",
        "subtitle": "Subtitle",
        "table_header": "Table Header",
        "footnote": "Footnote",
        "values": {
            "key1": { value: "1", units: "units", header: "Header 1" },
            "key2": { value: "1", units: "units", header: "Header 2" }
        }
    }
) {
    return make_popup(placeholders, tooltip_templates);
};

