const _layers = {
    "base": ["Light Theme"],
    "data": {
        "lcm_obs": {
            "layer": "Low-cost",
            "title": "Recent bias-corrected PM2.5 observations from Low-cost monitors (such as PurpleAir). Data are sourced from the PurpleAir API (Click for more info).",
            "link": "https://www2.purpleair.com/" // TODO: include "https://airqualityegg.com/"
        },
        "agency_obs": {
            "layer": "Regulatory",
            "title": "Recent observations from the gold-standard Agency (FEM) monitors in Canada. Data are sourced from the US AirNow database which individual Agencies push their data to (Click for more info).",
            "link": "https://fire.airnow.gov/"
        }
    }
};

const _default_layers = {
    "data": [0, 1], // same order as data_layers
    "base": 0 // same order as base_layers
} 