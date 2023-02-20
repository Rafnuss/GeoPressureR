library(testthat)
library(GeoPressureR)


# Small synthetic case
tag_sm <- list()
tag_sm$pressure <- data.frame(
  date = as.POSIXct(c(
    "2017-06-20 00:00:00 UTC", "2017-06-20 01:00:00 UTC",
    "2017-06-20 02:00:00 UTC", "2017-06-20 03:00:00 UTC"
  ), tz = "UTC"),
  value = c(1000, 1000, 1000, 1000),
  label = c("", "", "", "")
)
tag_sm <- tag_stap(tag_sm)
path_pressure_sm <- data.frame(
  lat = 46,
  lon = 16,
  stap = 1
)

test_that("geopressure_timeseries() | default output", {
  pressure_timeserie <- geopressure_timeseries(
    lon = 6, lat = 46,
    start_time = as.POSIXct("2017-06-20 00:00:00", tz = "UTC"),
    end_time = as.POSIXct("2017-06-20 02:00:00", tz = "UTC")
  )
  expect_s3_class(pressure_timeserie, "data.frame")
  expect_true(all(c("date", "pressure_era5", "lat", "lon") %in% names(pressure_timeserie)))
})

test_that("geopressure_timeseries() | on water", {
  # On water
  expect_warning(geopressure_timeseries(
    lon = 0, lat = 0,
    start_time = as.POSIXct("2017-06-20 00:00:00", tz = "UTC"),
    end_time = as.POSIXct("2017-06-20 02:00:00", tz = "UTC")
  ), "*water*")
})

test_that("geopressure_timeseries() | with lat, lon and pressure", {
  pressure_timeserie <- geopressure_timeseries(lon = 6, lat = 46, pressure = tag_sm$pressure)
  expect_s3_class(pressure_timeserie, "data.frame")
  expect_true(all(c("date", "pressure_era5") %in% names(pressure_timeserie)))
})

test_that("geopressure_timeseries_path() | Default output", {
  # Check for incorrect input
  expect_error(geopressure_timeseries_path(path_pressure_sm, "no_a_pressure"), "pressure is not a data frame")
  expect_error(geopressure_timeseries_path("not_a_path", tag_sm$pressure))
  expect_error(geopressure_timeseries_path(path_pressure_sm, tag_sm$pressure, include_flight = 2))
  expect_error(geopressure_timeseries_path(path_pressure_sm, tag_sm$pressure, include_flight = NA))
  expect_error(geopressure_timeseries_path(path_pressure_sm, tag_sm$pressure, include_flight = c(0, 10)))

  expect_no_error(geopressure_timeseries_path(path_pressure_sm, tag_sm$pressure))
})

test_that("geopressure_timeseries_path() | over water", {
  path_pressure_sm$lon <- 0
  path_pressure_sm$lat <- 0
  expect_warning(pressure_timeserie <- geopressure_timeseries_path(path_pressure_sm, tag_sm$pressure))
  expect_true(pressure_timeserie$lat[1] != 0)
})




# Start by computing all the necessary file for the tests
tag <- tag_read(
  directory = system.file("extdata/0_tag/18LX", package = "GeoPressureR"),
  crop_start = "2017-08-01", crop_end = "2017-10-01"
)
tag <- trainset_read(
  tag,
  directory = system.file("extdata/1_pressure/labels", package = "GeoPressureR")
)
tag <- tag_stap(tag)
pressure_mismatch <- readRDS(
  system.file(
    "extdata/1_pressure/18LX_pressure_mismatch.rds",
    package = "GeoPressureR"
  )
)
pressure_likelihood <- geopressure_likelihood(pressure_mismatch)
path_pressure <- map2path(pressure_likelihood)


i_s <- 4
n <- c(16, 32, 10) # number of valid datapoint for stap=4
path <- subset(path_pressure, stap == i_s)
pressure <- subset(tag$pressure, stap == i_s)

testthat("geopressure_timeseries() | full example", {
  pressure_timeserie <- geopressure_timeseries(path$lon, path$lat, pressure)
  expect_true(all(c(
    "date", "pressure_tag", "pressure_era5", "altitude", "pressure_era5_norm"
  ) %in% names(pressure_timeserie)))
  expect_equal(nrow(pressure_timeserie), n[2])
})

test_that("geopressure_timeseries_path() | full example", {
  pressure_timeserie <- expect_no_error(geopressure_timeseries_path(path, pressure))
  expect_true(all(c(
    "date", "pressure_tag", "stap", "pressure_era5", "altitude", "pressure_era5_norm", "lat", "lon",
    "stap_ref"
  ) %in% names(pressure_timeserie)))
  expect_equal(nrow(pressure_timeserie), n[2])
})

testthat("geopressure_timeseries() | include flight", {
  pressure <- subset(tag$pressure, stap == i_s | stap == 0)
  pressure_timeserie <- geopressure_timeseries(path$lon, path$lat, pressure)
  expect_equal(nrow(pressure_timeserie), 80)
})

testthat("geopressure_timeseries_path() | Flight single stap", {
  pressure_timeserie <- geopressure_timeseries_path(path, tag$pressure, include_flight = TRUE)
  expect_equal(nrow(pressure_timeserie), sum(n))
  expect_equal(sum(pressure_timeserie$stap == i_s), n[2])
  pressure_timeserie <- geopressure_timeseries_path(path, tag$pressure, include_flight = c(-1, 1))
  expect_equal(nrow(pressure_timeserie), sum(n[c(1, 3)]))
})

testthat("geopressure_timeseries_path() | Flight multiple stap", {
  path <- path_pressure[c(3, 4), ]
  pressure_timeserie <- geopressure_timeseries_path(path, tag$pressure)
  expect_gt(nrow(pressure_timeserie), n[2])
  expect_equal(sum(pressure_timeserie$stap == 4), n[2])
  pressure_timeserie <- geopressure_timeseries_path(path, pressure, include_flight = c(-1, 0, 1))
  expect_equal(sum(pressure_timeserie$stap_ref == 4), sum(n))
})
