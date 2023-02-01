library(testthat)
library(GeoPressureR)

# Start by computing all the necessary file for the tests
pam <- pam_read(
  pathname = system.file("extdata/0_PAM/18LX", package = "GeoPressureR"),
  crop_start = "2017-08-01", crop_end = "2017-10-01"
)
pam <- trainset_read(
  pam,
  pathname = system.file("extdata/1_pressure/labels", package = "GeoPressureR")
)
pam <- pam_sta(pam)

pressure_mismatch <- geopressure_mismatch(pam$pressure,
  extent = c(50, -16, 0, 23),
  scale = 10,
  max_sample = 100,
  margin = 30
)
pressure_likelihood <- geopressure_likelihood(pressure_mismatch)
path_pressure <- geopressure_map2path(pressure_likelihood)



test_that("Check geopressure_mismatch() for date too recent", {
  tmp <- Sys.time()
  pressure <- data.frame(
    date = c(tmp, tmp + 60 * 60, tmp + 2 * 60 * 60),
    obs = c(1000, 1000, 1000),
    sta_id = c(1, 1, 1)
  )
  expect_error(raster_list <- geopressure_mismatch(pressure, extent = c(1, 0, 0, 1), scale = 1))
})



test_that("Check geopressure_mismatch() timeout and worker", {
  expect_error(geopressure_mismatch(pressure, extent = c(1, 0, 0, 1), scale = 1, timeout=1))
  expect_error(geopressure_mismatch(pressure, extent = c(1, 0, 0, 1), scale = 1, worker=100))
})



test_that("Check geopressure_mismatch() output", {
  pressure <- data.frame(
    date = as.POSIXct(c(
      "2017-06-20 00:00:00 UTC", "2017-06-20 01:00:00 UTC",
      "2017-06-20 02:00:00 UTC"
    )),
    obs = c(1000, 1000, 1000),
    sta_id = c(1, 1, 1)
  )
  raster_list <- geopressure_mismatch(pressure, extent = c(1, 0, 0, 1), scale = 1)
  expect_type(raster_list, "list")
  expect_length(raster_list, 1)
  expect_s4_class(raster_list[[1]], "RasterBrick")

  # Check error for incorrect pressure
  pressure_tmp <- pressure
  pressure_tmp$obs[2] <- 1200
  expect_error(geopressure_mismatch(pressure_tmp, extent = c(1, 0, 0, 1), scale = 1))

  # Check error for incorrect pressure
  pressure_tmp <- pressure
  pressure_tmp$date[2] <- pressure$date[2] + 30 * 60
  expect_warning(geopressure_mismatch(pressure_tmp, extent = c(1, 0, 0, 1), scale = 1))

  # Check error for incorrect pressure
  pressure_tmp <- pressure
  pressure_tmp$sta_id <- 0
  expect_error(geopressure_mismatch(pressure_tmp, extent = c(1, 0, 0, 1), scale = 1))

  # Check back compatibility
  pressure_tmp <- pressure
  pressure_tmp$isoutliar <- FALSE
  expect_warning(geopressure_mismatch(pressure_tmp, extent = c(1, 0, 0, 1), scale = 1))
})



test_that("Check geopressure_likelihood() output", {
  expect_error(geopressure_likelihood(pressure_mismatch), NA)
  expect_error(geopressure_likelihood(pressure_mismatch, s = "not_a_number"))
  expect_error(geopressure_likelihood(pressure_mismatch, thr = "not_a_number"))
  expect_error(geopressure_likelihood(pressure_mismatch, fun_w = "not_a_function"))
})



test_that("Check geopressure_map2path() output", {
  # Error on incorrect parameter
  expect_error(geopressure_map2path(pressure_likelihood, interp = 2), NA)
  expect_error(geopressure_map2path(pressure_likelihood), NA)
  expect_error(geopressure_map2path(pressure_likelihood, format = "ind"), NA)
  expect_error(geopressure_map2path(pressure_likelihood, format = "arr.ind"), NA)

  # Not working without temporal_extent
  pressure_likelihood_tmp <- pressure_likelihood
  mt <- raster::metadata(pressure_likelihood_tmp[[1]])
  raster::metadata(pressure_likelihood_tmp[[1]]) <- mt[names(mt) %in% "temporal_extent" == FALSE]
  expect_error(geopressure_map2path(pressure_likelihood_tmp, interp = 2))

  # TODO: add flight

  # Check correct returned
  expect_s3_class(path_pressure, "data.frame")
  expect_true(all(c("lon", "lat", "sta_id") %in% names(path_pressure)))
  expect_gt(nrow(path_pressure), 0)
})



test_that("Check geopressure_timeseries() output", {
  pressure_timeserie <- geopressure_timeseries(
    lon = 6, lat = 46,
    start_time = as.POSIXct("2017-06-20 00:00:00", tz = "UTC"),
    end_time = as.POSIXct("2017-06-20 02:00:00", tz = "UTC")
  )
  expect_s3_class(pressure_timeserie, "data.frame")
  expect_true(all(c("date", "pressure", "lat", "lon") %in% names(pressure_timeserie)))

  # On water
  expect_warning(geopressure_timeseries(
    lon = 0, lat = 0,
    start_time = as.POSIXct("2017-06-20 00:00:00", tz = "UTC"),
    end_time = as.POSIXct("2017-06-20 02:00:00", tz = "UTC")
  ), "*water*")

  pressure <- data.frame(
    date = as.POSIXct(c(
      "2017-06-20 00:00:00", "2017-06-20 01:00:00",
      "2017-06-20 02:00:00"
    ), tz = "UTC"),
    obs = c(1000, 1000, 1000),
    sta_id = c(1, 1, 1)
  )
  pressure_timeserie <- geopressure_timeseries(lon = 6, lat = 46, pressure = pressure)
  expect_s3_class(pressure_timeserie, "data.frame")
  expect_true(all(c("date", "pressure") %in% names(pressure_timeserie)))

  # Check back compatibility
  pressure_tmp <- pressure
  pressure_tmp$isoutliar <- FALSE
  expect_warning(geopressure_timeseries(lon = 6, lat = 46, pressure = pressure_tmp))


  # i_s <- 4
  # n <- c(32, 32, 29)
  # path <- subset(path_pressure, sta_id == i_s)
  #
  # # Test Include flight
  # pressure <- subset(pam$pressure, sta_id == i_s)
  # pressure_timeserie <- geopressure_timeseries(path$lon, path$lat, pressure)
  # expect_true(all(c("date", "pressure", "altitude", "pressure0", "sta_id")
  # %in% names(pressure_timeserie)))
  # expect_equal(nrow(pressure_timeserie), n[2])
  #
  # pressure <- subset(pam$pressure, sta_id == i_s | sta_id == 0)
  # pressure_timeserie <- geopressure_timeseries(path$lon, path$lat, pressure)
  # expect_gt(nrow(pressure_timeserie), n[2])
})



test_that("Check geopressure_timeseries_path() output", {

  # Check for incorrect input
  expect_error(geopressure_timeseries_path(path_pressure, "no_a_pressure"))
  expect_error(geopressure_timeseries_path("not_a_path", pam$pressure))
  expect_error(geopressure_timeseries_path(path_pressure, pam$pressure, include_flight = 2))
  expect_error(geopressure_timeseries_path(path_pressure, pam$pressure, include_flight = NA))
  expect_error(geopressure_timeseries_path(path_pressure, pam$pressure, include_flight = c(0, 10)))

  i_s <- 4
  n <- c(16, 32, 10)
  pressure <- subset(pam$pressure, sta_id == i_s)
  path <- subset(path_pressure, sta_id == i_s)
  pressure_timeserie <- geopressure_timeseries_path(path, pressure)

  # Test Include flight
  pressure <- pam$pressure
  pressure_timeserie <- geopressure_timeseries_path(path, pressure)
  expect_equal(nrow(pressure_timeserie[[1]]), n[2])
  pressure_timeserie <- geopressure_timeseries_path(path, pressure, include_flight = TRUE)
  expect_equal(nrow(pressure_timeserie[[1]]), sum(n))
  expect_equal(sum(pressure_timeserie[[1]]$sta_id == i_s), n[2])
  pressure_timeserie <- geopressure_timeseries_path(path, pressure, include_flight = c(-1, 1))
  expect_equal(nrow(pressure_timeserie[[1]]), sum(n[c(1, 3)]))

  # test with multiple sta
  path <- path_pressure[c(3, 4), ]
  pressure_timeserie <- geopressure_timeseries_path(path, pressure)
  expect_equal(nrow(pressure_timeserie[[2]]), n[2])
  pressure_timeserie <- geopressure_timeseries_path(path, pressure, include_flight = c(-1, 0))
  expect_equal(nrow(pressure_timeserie[[2]]), sum(n[c(1, 2)]))

  # test on water
  path[1, ] <- c(0, 0, 4)
  expect_warning(pressure_timeserie <- geopressure_timeseries_path(path, pressure))
  expect_true(pressure_timeserie[[1]]$lat[1] != 0)

  # Check back compatibility
  pressure_tmp <- pressure
  pressure_tmp$isoutliar <- FALSE
  expect_warning(geopressure_timeseries_path(path, pressure_tmp))

  expect_warning(geopressure_timeseries_path(path, pressure))
})
