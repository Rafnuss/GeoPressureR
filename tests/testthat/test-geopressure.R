library(testthat)
library(GeoPressureR)

# Load data needed for the test
pressure_prob <- readRDS(system.file("extdata", "18LX_pressure_prob.rda", package = "GeoPressureR"))
static_prob <- readRDS(system.file("extdata", "18LX_static_prob.rda", package = "GeoPressureR"))
pathname <- system.file("extdata", package = "GeoPressureR")
pam_data <- pam_read(
  pathname = pathname,
  crop_start = "2017-06-20", crop_end = "2018-05-02"
)
pam_data <- trainset_read(pam_data, pathname = pathname)
pam_data <- pam_sta(pam_data)



test_that("Check geopressure_map() output", {
  pressure <- data.frame(
    date = as.POSIXct(c(
      "2017-06-20 00:00:00 UTC", "2017-06-20 01:00:00 UTC",
      "2017-06-20 02:00:00 UTC"
    )),
    obs = c(1000, 1000, 1000),
    sta_id = c(1, 1, 1)
  )
  raster_list <- geopressure_map(pressure, extent = c(1, 0, 0, 1), scale = 1)
  expect_type(raster_list, "list")
  expect_length(raster_list, 1)
  expect_s4_class(raster_list[[1]], "RasterBrick")
})



path_pressure <- geopressure_map2path(pressure_prob)
path_static <- geopressure_map2path(static_prob)
test_that("Check geopressure_map2path() output", {
  # Error on incorrect parameter
  expect_error(geopressure_map2path(pressure_prob, interp = 2), NA)
  expect_error(geopressure_map2path(pressure_prob, format = "ind"), NA)
  expect_error(geopressure_map2path(pressure_prob, format = "arr.ind"), NA)
  expect_error(geopressure_map2path(static_prob, interp = 100), NA)
  expect_error(geopressure_map2path(static_prob, format = "ind"), NA)
  expect_error(geopressure_map2path(static_prob, format = "arr.ind"), NA)
  expect_error(geopressure_map2path(static_prob, interp = 5, format = "ind"), NA)
  expect_error(geopressure_map2path(static_prob, interp = 5, format = "arr.ind"), NA)

  # Check correct returned
  expect_s3_class(path_pressure, "data.frame")
  expect_true(all(c("lon", "lat", "sta_id") %in% names(path_pressure)))
  expect_gt(nrow(path_pressure), 0)

  expect_s3_class(path_static, "data.frame")
  expect_true(all(c("lon", "lat", "sta_id") %in% names(path_static)))
  expect_gt(nrow(path_static), 0)
})





test_that("Check geopressure_ts() output", {
  pressure_timeserie <- geopressure_ts(
    lon = 6, lat = 46,
    start_time = as.POSIXct("2017-06-20 00:00:00 UTC"),
    end_time = as.POSIXct("2017-06-20 02:00:00 UTC")
  )
  expect_s3_class(pressure_timeserie, "data.frame")
  expect_true(all(c("date", "pressure") %in% names(pressure_timeserie)))

  pressure <- data.frame(
    date = as.POSIXct(c(
      "2017-06-20 00:00:00 UTC", "2017-06-20 01:00:00 UTC",
      "2017-06-20 02:00:00 UTC"
    )),
    obs = c(1000, 1000, 1000),
    sta_id = c(1, 1, 1)
  )
  pressure_timeserie <- geopressure_ts(lon = 6, lat = 46, pressure = pressure)
  expect_s3_class(pressure_timeserie, "data.frame")
  expect_true(all(c("date", "pressure") %in% names(pressure_timeserie)))


  skip_if(TRUE, "Heavy tests, run only manually")

  i_s <- 8
  n <- c(29, 19, 29)
  path <- subset(path_static, sta_id == i_s)

  # Test Include flight
  pressure <- subset(pam_data$pressure, sta_id == i_s)
  pressure_timeserie <- geopressure_ts(path$lon, path$lat, pressure)
  expect_true(all(c("date", "pressure", "altitude", "pressure0", "sta_id")
                  %in% names(pressure_timeserie)))
  expect_equal(nrow(pressure_timeserie), n[2])

  pressure <- subset(pam_data$pressure, sta_id == i_s | sta_id == 0)
  pressure_timeserie <- geopressure_ts(path$lon, path$lat, pressure)
  expect_gt(nrow(pressure_timeserie), n[2])
})




test_that("Check geopressure_ts_path() output", {
  skip_if(TRUE, "Heavy tests, run only manually")

  # Check for incorrect input
  expect_error(geopressure_ts_path(path_static, "no_a_pressure"))
  expect_error(geopressure_ts_path("not_a_path", pam_data$pressure))
  expect_error(geopressure_ts_path(path_static, pam_data$pressure, include_flight = 2))
  expect_error(geopressure_ts_path(path_static, pam_data$pressure, include_flight = NA))
  expect_error(geopressure_ts_path(path_static, pam_data$pressure, include_flight = c(0, 10)))

  i_s <- 8
  n <- c(29, 19, 29)
  pressure <- subset(pam_data$pressure, sta_id == i_s)
  path <- subset(path_static, sta_id == i_s)
  pressure_timeserie <- geopressure_ts_path(path, pressure)

  # Test Include flight
  pressure <- pam_data$pressure
  pressure_timeserie <- geopressure_ts_path(path, pressure)
  expect_equal(nrow(pressure_timeserie[[1]]), n[2])
  pressure_timeserie <- geopressure_ts_path(path, pressure, include_flight = T)
  expect_equal(nrow(pressure_timeserie[[1]]), sum(n))
  expect_equal(sum(pressure_timeserie[[1]]$sta_id == i_s), n[2])
  pressure_timeserie <- geopressure_ts_path(path, pressure, include_flight = c(-1, 1))
  expect_equal(nrow(pressure_timeserie[[1]]), sum(n[c(1, 3)]))

  # test with multiple sta
  path <- path_static[c(8, 9), ]
  pressure_timeserie <- geopressure_ts_path(path, pressure)
  expect_equal(nrow(pressure_timeserie[[1]]), n[2])
  pressure_timeserie <- geopressure_ts_path(path, pressure, include_flight = c(-1, 0))
  expect_equal(nrow(pressure_timeserie[[1]]), sum(n[c(1, 2)]))
})
