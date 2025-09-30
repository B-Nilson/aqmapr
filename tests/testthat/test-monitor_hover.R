test_that("make_monitor_hover() works", {
  result <- make_monitor_hover(
    name = "name",
    network = factor("agency", levels = c("agency", "lcm")),
    monitor_type = factor("FEM", levels = c("FEM", "PA", "EGG")),
    date_last_obs = lubridate::ymd_h("2020-01-01 01"),
    pm25_10min = 1,
    pm25_1hr = 2,
    pm25_3hr = 3,
    pm25_24hr = 4
  )
  expected <- paste0(
    "<big><b>name</b></big><br>",
    "Type: <b>Regulatory</b><br>",
    "Monitor: <b>Federal Equivelant Method (FEM)</b><br>",
    "Time: <b>2020-01-01T01:00:00Z</b><br>",
    "<table>",
    "<tr><th>PM<sub>2.5</sub> averages:</th></tr>",
    "<tr><td style='width: 1%;'>1 hr.:</td><td><b>2</b> &mu;g m <sup>-3</sup></td></tr>",
    "<tr><td style='width: 1%;'>3 hr.:</td><td><b>3</b> &mu;g m <sup>-3</sup></td></tr>",
    "<tr><td style='width: 1%;'>24 hr.:</td><td><b>4</b> &mu;g m <sup>-3</sup></td></tr>",
    "</table>"
  )
  expect_equal(result, expected)
})

test_that("make_pm_summary_table() works", {
  result <- make_pm_summary_table(
    network = factor("agency", levels = c("agency", "lcm")),
    pm25_10min = 1,
    pm25_1hr = 2,
    pm25_3hr = 3,
    pm25_24hr = 4
  )
  expected <- paste0(
    "<table>",
    "<tr><th>PM<sub>2.5</sub> averages:</th></tr>",
    "<tr><td style='width: 1%;'>1 hr.:</td><td><b>2</b> &mu;g m <sup>-3</sup></td></tr>",
    "<tr><td style='width: 1%;'>3 hr.:</td><td><b>3</b> &mu;g m <sup>-3</sup></td></tr>",
    "<tr><td style='width: 1%;'>24 hr.:</td><td><b>4</b> &mu;g m <sup>-3</sup></td></tr>",
    "</table>"
  )
  expect_equal(result, expected)
})

test_that("make_obs_table_row() works", {
  table_row <- make_obs_table_row(
    label = "Value:",
    value = 1,
    units = "cm",
    missing_text = "No data"
  )
  expected <- "<tr><td style='width: 1%;'>Value:</td><td><b>1</b> cm</td></tr>"
  expect_equal(table_row, expected)
})
