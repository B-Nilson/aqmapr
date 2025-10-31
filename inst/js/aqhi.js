aqhi_health_messages = {
    'No Data': 'Data for the past hour from this monitor is missing',
    'Low': 'General Population - Ideal air for outdoor activities. At Risk - Enjoy usual outdoor activities.',
    'Moderate': 'General Population - No need to modify your usual outdoor activities unless you experience symptoms such as coughing and throat irritation. At Risk - Consider reducing or rescheduling strenuous activities outdoors if you are experiencing symptoms.',
    'High': 'General Population - Consider reducing or rescheduling strenuous activities outdoors if you experience symptoms such as coughing and throat irritation. At Risk - Reduce or reschedule strenuous activities outdoors. Children and the elderly should also take it easy.',
    'Very High': 'General Population - Reduce or reschedule strenuous activities outdoors, especially if you experience symptoms such as coughing and throat irritation. At Risk - Avoid strenuous activities outdoors. Children and the elderly should also avoid outdoor physical exertion.'
};

function get_aqhi_category(pm25_1hr = -1) {
    if (pm25_1hr < 0) {
        return 'No Data';
    } else if (pm25_1hr < 30) {
        return 'Low';
    } else if (pm25_1hr < 60) {
        return 'Moderate';
    } else if (pm25_1hr < 100) {
        return 'High';
    } else {
        return 'Very High';
    }
}
