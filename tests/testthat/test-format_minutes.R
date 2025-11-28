library(testthat)
library(GeoPressureR)

test_that("format_minutes() formats minutes into readable durations", {
  expect_equal(GeoPressureR:::format_minutes(0), "0min")
  expect_equal(GeoPressureR:::format_minutes(59), "59min")
  expect_equal(GeoPressureR:::format_minutes(61), "1h 1min")
  expect_equal(GeoPressureR:::format_minutes(24 * 60 + 2), "1d 0h 2min")
  expect_equal(GeoPressureR:::format_minutes(365 * 24 * 60 + 90), "1y 1h 30min")

  expect_equal(GeoPressureR:::format_minutes(90.6), "1h 31min")
})
