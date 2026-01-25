async function load_html_templates(paths) {
    const entries = Object.entries(paths);

    const loaded = await Promise.all(
        entries.map(async ([key, path]) => {
            const text = await fetch(path).then(r => r.text());
            return [key, text]; // return key + template content
        })
    );

    // convert array back to dict
    return Object.fromEntries(loaded);
}

const popup_template_dir = "/html"
const popup_template_paths = {
    template: popup_template_dir + "/monitor_popup.html",
    row: popup_template_dir + "/monitor_popup_table_row.html"
};
let popup_templates = load_html_templates(popup_template_paths);

async function make_monitor_popup(
    placeholders = { station_name: "Station Name", monitor_type: "Monitor Type", date_stamp: "2025-01-01T01:00:00Z", health_message: "Health message." },
    table_values = { pm25_10min: -1, pm25_1hr: -1, pm25_3hr: -1, pm25_24hr: -1 },
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
    let popup_placeholders = {
        "title": title,
        "subtitle": subtitle,
        "table_header": table_header,
        "footnote": footnote,
        "values": {}
    };
    for (let key in table_values) {
        popup_placeholders.values[key] = {
            value: table_values[key] ?? null_text,
            units: table_units[key],
            header: table_labels[key]
        };
    }

    // Build popup from html templates in html/
    try {
        let popup = await make_popup(popup_placeholders);
        return popup;
    } catch (error) {
        console.error("Error making monitor popup:", error);
    }
}

async function make_popup(
    placeholders = {
        "title": "Title",
        "subtitle": "Subtitle",
        "table_header": "Table Header",
        "footnote": "Footnote",
        "values": {
            "key1": { value: "1", units: "units", header: "Header 1" },
            "key2": { value: "1", units: "units", header: "Header 2" }
        }
    },
    templates = popup_templates
) {
    templates = await templates;
    // Build values table for popup
    let popup_table_rows = Object.keys(placeholders.values).map(
        (key) => {
            return replace_placeholders(templates.row, placeholders.values[key]);
        });
    placeholders.table_rows = popup_table_rows.join("");
    return replace_placeholders(templates.template, placeholders);
};

// Replace all {{placeholders}} with name values in a dict
// i.e. replace_placeholders("Hello {{name}}, you are {{age}} years old", { "name": "John Doe", "age": 30 })
function replace_placeholders(template, placeholders) {
    return template.replaceAll(/{{(.*?)}}/g, (match, key) => {
        const value = placeholders[key.trim()];
        return value ? value : "";
    });
};
