library(testthat)
library(GeoPressureR)

# Small synthetic case
tag_sm <- structure(list(), class = "tag")
tag_sm$pressure <- data.frame(
  date = as.POSIXct(c(
    "2017-06-20 00:00:00 UTC", "2017-06-20 01:00:00 UTC",
    "2017-06-20 02:00:00 UTC", "2017-06-20 03:00:00 UTC"
  ), tz = "UTC"),
  value = c(1000, 1000, 1000, 1000),
  label = c("", "", "", "")
)
tag_sm <- tag_label_stap(tag_sm, quiet = TRUE)
path_sm <- data.frame(
  lat = 46,
  lon = 16,
  stap_id = 1
)

test_that("geopressure_timeseries() | default output", {
  pressure_timeseries <- geopressure_timeseries(
    lon = 6, lat = 46,
    start_time = as.POSIXct("2017-06-20 00:00:00", tz = "UTC"),
    end_time = as.POSIXct("2017-06-20 02:00:00", tz = "UTC"),
    quiet = TRUE
  )
  expect_s3_class(pressure_timeseries, "data.frame")
  expect_true(all(c("date", "surface_pressure", "lat", "lon") %in% names(pressure_timeseries)))
})

test_that("geopressure_timeseries() | with lat, lon and pressure", {
  pressure_timeseries <- geopressure_timeseries(lat = 46, lon = 6, pressure = tag_sm$pressure, quiet = TRUE)
  expect_s3_class(pressure_timeseries, "data.frame")
  expect_true(all(c("date", "surface_pressure") %in% names(pressure_timeseries)))
})
