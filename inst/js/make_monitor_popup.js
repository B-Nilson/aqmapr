
const popup_template_dir = "/html"
const popup_template_paths = {
    popup: popup_template_dir +  "/monitor_popup.html",
    row: popup_template_dir +  "/monitor_popup_table_row.html"
};
let popup_templates = load_html_templates(popup_template_paths);

async function make_monitor_popup(
    station_name,
    monitor_type,
    values = { date_stamp: "2025-01-01T01:00:00Z", pm25_1hr: -1, pm25_3hr: -1, pm25_24hr: -1 }
) {
    let pm25_units = "&mu;g m <sup>-3</sup>";
    let placeholders = {
        "title": station_name,
        "subtitle": monitor_type + " monitor",
        "table_header": "Observed PM<sub>2.5</sub> as of: " + values.date_stamp,
        "footnote": aqhi_health_messages[get_aqhi_category(values.pm25_1hr)],
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

async function make_popup(
    placeholders = {
        "title": "Title",
        "subtitle": "Subtitle",
        "table_header": "Table Header",
        "footnote": "Footnote",
        "values": {
            "name1": { value: "1", units: "units" },
            "name2": { value: "1", units: "units" }
        }
    }
) {
    let templates = await popup_templates;
    // Replace placeholders in the templates
    let popup_rows = Object.keys(placeholders.values).map((name) => {
        let value = placeholders.values[name];
        return replace_placeholders(templates.row, { "header": name, "value": value.value, "units": value.units });
    });

    let popup = replace_placeholders(
        templates.popup,
        Object.assign(placeholders, { "table_rows": popup_rows.join("") })
    );
    return popup
};

// Replace all {{placeholders}} with name values in a dict
// i.e. replace_placeholders("Hello {{name}}, you are {{age}} years old", { "name": "John Doe", "age": 30 })
function replace_placeholders(template, placeholders) {
    return template.replaceAll(/{{(.*?)}}/g, (match, key) => {
        const value = placeholders[key.trim()];
        return value ? value : "";
    });
};

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