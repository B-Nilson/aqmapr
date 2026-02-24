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

function replace_date_placeholder(text, date_format, tz, en_francais = false) {
    const pattern = /\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z/g;
    const dates = text.match(pattern);
    if (!dates) return text;

    const locale = en_francais ? "fr-CA" : "en-CA";

    const formatOptions = buildFormatOptions(date_format);

    const formatter = new Intl.DateTimeFormat(locale, {
        ...formatOptions,
        timeZone: tz
    });

    return dates.reduce((acc, dateStr) => {
        const date = new Date(dateStr);
        const formattedDate = formatter.format(date);
        return acc.replace(dateStr, `${formattedDate} ${tz}`);
    }, text);
}


/**
 * Minimal token mapper to approximate Highcharts.dateFormat
 * Extend as needed.
 */
function buildFormatOptions(format) {
    const options = {};

    if (format.includes('%Y')) options.year = 'numeric';
    if (format.includes('%y')) options.year = '2-digit';
    if (format.includes('%B')) options.month = 'long';
    else if (format.includes('%b')) options.month = 'short';
    else if (format.includes('%m')) options.month = '2-digit';

    if (format.includes('%e') || format.includes('%d')) options.day = '2-digit';
    if (format.includes('%H')) options.hour = '2-digit';
    if (format.includes('%M')) options.minute = '2-digit';
    if (format.includes('%S')) options.second = '2-digit';

    return options;
}
