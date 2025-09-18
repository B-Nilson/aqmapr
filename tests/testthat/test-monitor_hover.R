test_that("make_monitor_hover() works", {
  result <- make_monitor_hover(
    name = "name",
    network = "agency",
    date_last_obs = lubridate::ymd_h("2020-01-01 01"),
    pm25_10min = 1,
    pm25_1hr = 2,
    pm25_3hr = 3,
    pm25_24hr = 4
  )
  expected <- paste0(
    "<big><b>name</b></big><br>",
    "Type: <b>Regulatory</b><br>",
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
    network = "agency",
    pm25_10min = 1,
    pm25_1hr = 2,
    pm25_3hr = 3,
    pm25_24hr = 4,
    text = list(
      pm_title = "PM<sub>2.5</sub> averages:",
      pm_10min = "10 min.:",
      pm_1hr = "1 hr.:",
      pm_3hr = "3 hr.:",
      pm_24hr = "24 hr.:",
      no_data = "No Data."
    )
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
