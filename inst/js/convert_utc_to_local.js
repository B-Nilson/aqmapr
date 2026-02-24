format_map_timestamp = function (hover_text = null, remove_transparency = true, date_format = "%Y %b %d %H:%M", tz = "browser", en_francais = false) {
    if (tz === "browser") tz = get_browser_timezone(en_francais);
    let timestamp_div = document.getElementById('map_timestamp');
    if (!timestamp_div) return
    // Replace UTC date placeholder with local time prended with `prefix`
    let timestamp = replace_date_placeholder(timestamp_div.innerHTML, date_format, tz, en_francais);
    timestamp_div.innerHTML = timestamp;
    // Handle extra options
    if (hover_text) timestamp_div.title = hover_text;
    if (remove_transparency) timestamp_div.style.backgroundColor = "rgba(255, 255, 255)";
}

get_browser_timezone = function (en_francais = false) {
    const options = { timeZoneName: 'short' };
    const locale = en_francais ? 'fr-ca' : 'en-us';
    const timeString = new Date().toLocaleTimeString(locale, options);
    const timeZoneIndex = en_francais ? 6 : 2;
    return timeString.split(' ')[timeZoneIndex];
}

replace_date_placeholder = function (text, date_format, tz, en_francais = false) {
    Highcharts.setOptions({
        time: { useUTC: true, timezoneOffset: (new Date().getTimezoneOffset()) }
    });
    if (en_francais) {
        Highcharts.setOptions({
            lang: {
                shortMonths: ["janv.", "févr.", "mars", "avr.", "mai", "juin", "juil.", "août", "sept.", "oct.", "nov.", "déc."]
            }
        });
    }
    const pattern = /\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z/;
    const regex = new RegExp(pattern, 'g');
    const dates = text.match(regex);
    if (dates) {
        return dates.reduce((acc, date) => {
            const formattedDate = Highcharts.dateFormat(date_format, new Date(date).getTime());
            return acc.replace(date, formattedDate + ' ' + tz);
        }, text);
    }
}
