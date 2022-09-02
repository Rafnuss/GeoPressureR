library(testthat)
library(GeoPressureR)

pam <- pam_read(
  pathname = system.file("extdata/0_PAM/18LX", package = "GeoPressureR"),
  crop_start = "2017-08-01", crop_end = "2017-10-01"
)
pam <- trainset_read(
  pam,
  pathname = system.file("extdata/1_pressure/labels", package = "GeoPressureR")
)
pam <- pam_sta(pam)


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

pressure_maps <- geopressure_map(pam$pressure,
  extent = c(50, -16, 0, 23),
  scale = 10,
  max_sample = 100,
  margin = 30
)


test_that("Check geopressure_prob_map() output", {
  expect_error(geopressure_prob_map(pressure_maps), NA)
  expect_error(geopressure_prob_map(pressure_maps, s = "not_a_number"))
  expect_error(geopressure_prob_map(pressure_maps, thr = "not_a_number"))
  expect_error(geopressure_prob_map(pressure_maps, fun_w = "not_a_function"))
})
pressure_prob <- geopressure_prob_map(pressure_maps)

path_pressure <- geopressure_map2path(pressure_prob)

test_that("Check geopressure_map2path() output", {
  # Error on incorrect parameter
  expect_error(geopressure_map2path(pressure_prob, interp = 2), NA)
  expect_error(geopressure_map2path(pressure_prob, format = "ind"), NA)
  expect_error(geopressure_map2path(pressure_prob, format = "arr.ind"), NA)

  # Check correct returned
  expect_s3_class(path_pressure, "data.frame")
  expect_true(all(c("lon", "lat", "sta_id") %in% names(path_pressure)))
  expect_gt(nrow(path_pressure), 0)
})





test_that("Check geopressure_ts() output", {
  pressure_timeserie <- geopressure_ts(
    lon = 6, lat = 46,
    start_time = as.POSIXct("2017-06-20 00:00:00", tz = "UTC"),
    end_time = as.POSIXct("2017-06-20 02:00:00", tz = "UTC")
  )
  expect_s3_class(pressure_timeserie, "data.frame")
  expect_true(all(c("date", "pressure", "lat", "lon") %in% names(pressure_timeserie)))

  # On water
  expect_warning(geopressure_ts(
    lon = 0, lat = 0,
    start_time = as.POSIXct("2017-06-20 00:00:00", tz = "UTC"),
    end_time = as.POSIXct("2017-06-20 02:00:00", tz = "UTC")
  ), "*water*")
  expect_s3_class(pressure_timeserie, "data.frame")
  expect_true(all(c("date", "pressure", "lat", "lon") %in% names(pressure_timeserie)))


  pressure <- data.frame(
    date = as.POSIXct(c(
      "2017-06-20 00:00:00", "2017-06-20 01:00:00",
      "2017-06-20 02:00:00"
    ), tz = "UTC"),
    obs = c(1000, 1000, 1000),
    sta_id = c(1, 1, 1)
  )
  pressure_timeserie <- geopressure_ts(lon = 6, lat = 46, pressure = pressure)
  expect_s3_class(pressure_timeserie, "data.frame")
  expect_true(all(c("date", "pressure") %in% names(pressure_timeserie)))

  # i_s <- 4
  # n <- c(32, 32, 29)
  # path <- subset(path_pressure, sta_id == i_s)
  #
  # # Test Include flight
  # pressure <- subset(pam$pressure, sta_id == i_s)
  # pressure_timeserie <- geopressure_ts(path$lon, path$lat, pressure)
  # expect_true(all(c("date", "pressure", "altitude", "pressure0", "sta_id")
  # %in% names(pressure_timeserie)))
  # expect_equal(nrow(pressure_timeserie), n[2])
  #
  # pressure <- subset(pam$pressure, sta_id == i_s | sta_id == 0)
  # pressure_timeserie <- geopressure_ts(path$lon, path$lat, pressure)
  # expect_gt(nrow(pressure_timeserie), n[2])
})




test_that("Check geopressure_ts_path() output", {

  # Check for incorrect input
  expect_error(geopressure_ts_path(path_pressure, "no_a_pressure"))
  expect_error(geopressure_ts_path("not_a_path", pam$pressure))
  expect_error(geopressure_ts_path(path_pressure, pam$pressure, include_flight = 2))
  expect_error(geopressure_ts_path(path_pressure, pam$pressure, include_flight = NA))
  expect_error(geopressure_ts_path(path_pressure, pam$pressure, include_flight = c(0, 10)))

  i_s <- 4
  n <- c(16, 32, 10)
  pressure <- subset(pam$pressure, sta_id == i_s)
  path <- subset(path_pressure, sta_id == i_s)
  pressure_timeserie <- geopressure_ts_path(path, pressure)

  # Test Include flight
  pressure <- pam$pressure
  pressure_timeserie <- geopressure_ts_path(path, pressure)
  expect_equal(nrow(pressure_timeserie[[1]]), n[2])
  pressure_timeserie <- geopressure_ts_path(path, pressure, include_flight = TRUE)
  expect_equal(nrow(pressure_timeserie[[1]]), sum(n))
  expect_equal(sum(pressure_timeserie[[1]]$sta_id == i_s), n[2])
  pressure_timeserie <- geopressure_ts_path(path, pressure, include_flight = c(-1, 1))
  expect_equal(nrow(pressure_timeserie[[1]]), sum(n[c(1, 3)]))

  # test with multiple sta
  path <- path_pressure[c(3, 4), ]
  pressure_timeserie <- geopressure_ts_path(path, pressure)
  expect_equal(nrow(pressure_timeserie[[2]]), n[2])
  pressure_timeserie <- geopressure_ts_path(path, pressure, include_flight = c(-1, 0))
  expect_equal(nrow(pressure_timeserie[[2]]), sum(n[c(1, 2)]))

  # test on water
  path[1, ] <- c(0, 0, 4)
  expect_warning(pressure_timeserie <- geopressure_ts_path(path, pressure))
  expect_true(pressure_timeserie[[1]]$lat[1] != 0)
})
