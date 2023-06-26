library(testthat)
library(GeoPressureR)
# Hide cli message
# options(cli.default_handler = function(...) { })

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
tag_sm <- tag_label_stap(tag_sm)
path_pressure_sm <- data.frame(
  lat = 46,
  lon = 16,
  stap_id = 1
)

test_that("geopressure_timeseries_latlon() | default output", {
  pressure_timeseries <- geopressure_timeseries_latlon(
    lon = 6, lat = 46,
    start_time = as.POSIXct("2017-06-20 00:00:00", tz = "UTC"),
    end_time = as.POSIXct("2017-06-20 02:00:00", tz = "UTC")
  )
  expect_s3_class(pressure_timeserie, "data.frame")
  expect_true(all(c("date", "pressure_era5", "lat", "lon") %in% names(pressure_timeseries)))
})

test_that("geopressure_timeseries_latlon() | on water", {
  # On water
  expect_warning(geopressure_timeseries_latlon(
    lon = 0, lat = 0,
    start_time = as.POSIXct("2017-06-20 00:00:00", tz = "UTC"),
    end_time = as.POSIXct("2017-06-20 02:00:00", tz = "UTC")
  ), "*water*")
})

test_that("geopressure_timeseries_latlon() | with lat, lon and pressure", {
  pressure_timeseries <- geopressure_timeseries_latlon(lat = 46, lon = 6, pressure = tag_sm$pressure)
  expect_s3_class(pressure_timeserie, "data.frame")
  expect_true(all(c("date", "pressure_era5") %in% names(pressure_timeseries)))
})

test_that("geopressure_timeseries() | Default output", {
  # Check for incorrect input
  expect_error(geopressure_timeseries(path_pressure_sm, "no_a_pressure"), "pressure is not a data frame")
  expect_error(geopressure_timeseries("not_a_path", tag_sm$pressure))
  expect_error(geopressure_timeseries(path_pressure_sm, tag_sm$pressure, include_flight = 2))
  expect_error(geopressure_timeseries(path_pressure_sm, tag_sm$pressure, include_flight = NA))
  expect_error(geopressure_timeseries(path_pressure_sm, tag_sm$pressure, include_flight = c(0, 10)))

  expect_no_error(geopressure_timeseries(path_pressure_sm, tag_sm$pressure))
})

test_that("geopressure_timeseries() | over water", {
  path_pressure_sm$lon <- 0
  path_pressure_sm$lat <- 0
  expect_warning(pressure_timeseries <- geopressure_timeseries(path_pressure_sm, tag_sm$pressure))
  expect_true(pressure_timeseries$lat[1] != 0)
})






# Start by computing all the necessary file for the tests
tag <- tag_create("18LX") |> tag_label()
stap <- tag$stap
path <- data.frame(
  stap_id = seq_len(5),
  ind = c(1652, 1554, 1407, 1611, 1409),
  lat = c(48.5, 46.5, 43.5, 39.5, 41.5),
  lon = c(17.5, 15.5, 12.5, 16.5, 12.5),
  model = c(T, T, T, T, T),
  known = c(T, F, F, F, F)
)

i_s <- 4
n <- c(16, 32, 10) # number of valid datapoint for stap=4
path_i <- subset(path, stap_id == i_s)
pressure <- subset(tag$pressure, stap_id == i_s)

test_that("geopressure_timeseries_latlon() | full example", {
  path_pres <- geopressure_timeseries_latlon(path_i$lat, path_i$lon, pressure)
  expect_true(all(c(
    "date", "pressure_tag", "pressure_era5", "altitude", "pressure_era5_norm"
  ) %in% names(path_pres)))
  expect_equal(nrow(path_pres), n[2])
})

test_that("geopressure_timeseries() | full example", {
  path_pres <- expect_no_error(geopressure_timeseries(path_i, pressure))
  expect_true(all(c(
    "date", "pressure_tag", "stap_id", "pressure_era5", "altitude", "pressure_era5_norm", "lat",
    "stap_ref", "lon"
  ) %in% names(path_pres)))
  expect_equal(nrow(path_pres), n[2])
})

test_that("geopressure_timeseries_latlon() | include flight", {
  pressure <- subset(tag$pressure, stap == i_s | stap == 0)
  path_pres <- geopressure_timeseries_latlon(path_i$lat, path_i$lon, pressure)
  expect_equal(nrow(path_pres), 80)
})

test_that("geopressure_timeseries() | Flight single stap", {
  path_pres <- geopressure_timeseries(path_i, tag$pressure, include_flight = TRUE)
  expect_equal(nrow(path_pres), sum(n))
  expect_equal(sum(path_pres$stap_id == i_s), n[2])
  path_pres <- geopressure_timeseries(path_i, tag$pressure, include_flight = c(-1, 1))
  expect_equal(nrow(path_pres), sum(n[c(1, 3)]))
})

test_that("geopressure_timeseries() | Flight multiple stap", {
  path_i <- path[c(2, 4), ]
  path_pres <- geopressure_timeseries(path, tag$pressure)
  expect_equal(sum(path_pres$stap_id == 4), n[2])
})
