library(testthat)
library(GeoPressureR)
# Hide cli message
options(cli.default_handler = function(...) { })

# Set working directory
setwd(system.file("extdata", package = "GeoPressureR"))

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
expect_warning(tag_sm <- tag_label_stap(tag_sm))
path_sm <- data.frame(
  lat = 46,
  lon = 16,
  stap_id = 1
)

test_that("geopressure_timeseries() | default output", {
  pressure_timeseries <- geopressure_timeseries(
    lon = 6, lat = 46,
    start_time = as.POSIXct("2017-06-20 00:00:00", tz = "UTC"),
    end_time = as.POSIXct("2017-06-20 02:00:00", tz = "UTC")
  )
  expect_s3_class(pressure_timeseries, "data.frame")
  expect_true(all(c("date", "pressure_era5", "lat", "lon") %in% names(pressure_timeseries)))
})

test_that("geopressure_timeseries() | with lat, lon and pressure", {
  pressure_timeseries <- geopressure_timeseries(lat = 46, lon = 6, pressure = tag_sm$pressure)
  expect_s3_class(pressure_timeseries, "data.frame")
  expect_true(all(c("date", "pressure_era5") %in% names(pressure_timeseries)))
})

test_that("pressurepath_create() | Default output", {
  # Check for incorrect input
  expect_error(pressurepath_create(tag_sm, path_sm, include_flight = 2))
  expect_error(pressurepath_create(tag_sm, path_sm, include_flight = NA))
  expect_error(pressurepath_create(tag_sm, path_sm, include_flight = c(0, 10)))

  expect_no_error(pressurepath_create(tag_sm, path_sm))
})






# Start by computing all the necessary file for the tests
tag <- tag_create("18LX", quiet = TRUE) |> tag_label(quiet = TRUE)
stap <- tag$stap
path <- data.frame(
  stap_id = seq_len(5),
  ind = c(1652, 1554, 1407, 1611, 1409),
  lat = c(48.5, 46.5, 43.5, 39.5, 41.5),
  lon = c(17.5, 15.5, 12.5, 16.5, 13),
  model = c(TRUE, TRUE, TRUE, TRUE, TRUE),
  known = c(TRUE, FALSE, FALSE, FALSE, FALSE)
)

i_s <- 4
n <- c(16, 32, 10) # number of valid datapoint for stap=4
path_i <- subset(path, stap_id == i_s)
pressure <- subset(tag$pressure, stap_id == i_s)

test_that("geopressure_timeseries() | full example", {
  pressurepath <- geopressure_timeseries(path_i$lat, path_i$lon, pressure)
  expect_true(all(c(
    "date", "pressure_tag", "pressure_era5", "altitude", "pressure_era5_norm"
  ) %in% names(pressurepath)))
  expect_equal(nrow(pressurepath), n[2])
})

test_that("pressurepath_create() | full example", {
  pressurepath <- expect_no_error(pressurepath_create(tag, path_i))
  expect_true(all(c(
    "date", "pressure_tag", "stap_id", "pressure_era5", "altitude", "pressure_era5_norm", "lat",
    "stap_ref", "lon"
  ) %in% names(pressurepath)))
  expect_equal(nrow(pressurepath), n[2])
})

test_that("geopressure_timeseries() | include flight", {
  pressure <- subset(tag$pressure, stap == i_s | stap == 0)
  pressurepath <- geopressure_timeseries(path_i$lat, path_i$lon, pressure)
  expect_equal(nrow(pressurepath), 45)
})

test_that("pressurepath_create() | Flight single stap", {
  pressurepath <- pressurepath_create(tag, path_i, include_flight = TRUE)
  expect_equal(nrow(pressurepath), sum(n))
  expect_equal(sum(pressurepath$stap_id == i_s), n[2])
  pressurepath <- pressurepath_create(tag, path_i, include_flight = c(-1, 1))
  expect_equal(nrow(pressurepath), sum(n[c(1, 3)]))
})

test_that("pressurepath_create() | Flight multiple stap", {
  path_i <- path[c(2, 4), ]
  pressurepath <- pressurepath_create(tag, path_i)
  expect_equal(sum(pressurepath$stap_id == 4), n[2])
})
