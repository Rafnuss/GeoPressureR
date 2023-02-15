library(testthat)
library(GeoPressureR)


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
pressure_mismatch <- geopressure_mismatch(tag,
  extent = c(-16, 23, 0, 50),
  scale = 1,
  max_sample = 100,
  margin = 30
)
pressure_likelihood <- geopressure_likelihood(pressure_mismatch)
path_pressure <- map2path(pressure_likelihood)


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


test_that("Check geopressure_mismatch() output", {
  mismatch <- geopressure_mismatch(tag_sm, extent = c(0, 1, 0, 1), scale = 1)
  expect_type(mismatch, "list")
  expect_length(mismatch, 1)
  expect_type(mismatch[[1]], "list")
  expect_true(all(c("mse", "mask", "stap", "nb_sample", "start", "end") %in% names(mismatch[[1]])))
  expect_equal(length(dim(mismatch[[1]]$mse)), 2)
  expect_equal(length(dim(mismatch[[1]]$mask)), 2)
  expect_true(mismatch[[1]]$stap == 1)
  expect_true(mismatch[[1]]$nb_sample == 4)
  expect_true(mismatch[[1]]$start == tag_sm$pressure$date[1])
  expect_true(mismatch[[1]]$end == tag_sm$pressure$date[4])


  # Check error for incorrect pressure
  tag_sm_2 <- tag_sm
  tag_sm_2$pressure$value[2] <- 1200
  expect_error(
    geopressure_mismatch(tag_sm_2, extent = c(0, 1, 0, 1), scale = 1),
    "*Pressure observation should be between 250 hPa*"
  )

  # Check warning for irregular date
  tag_sm_2 <- tag_sm
  tag_sm_2$pressure$date[3] <- tag_sm_2$pressure$date[3] + 1
  expect_warning(
    geopressure_mismatch(tag_sm_2, extent = c(0, 1, 0, 1), scale = 1),
    "*The pressure data is not on a regular interval*"
  )

  # Check error for date too recent
  tag_sm_2 <- tag_sm
  tmp <- Sys.time()
  tag_sm_2$pressure$date <- tmp + c(0, 1, 2, 3) * 60 * 60
  expect_error(
    geopressure_mismatch(tag_sm_2, extent = c(0, 1, 0, 1), scale = 1),
    "*made for periods where no data are available*"
  )

  # Check geopressure_mismatch() timeout and worker
  expect_error(geopressure_mismatch(tag, extent = c(0, 1, 0, 1), scale = 1, timeout = 1))
  expect_error(geopressure_mismatch(tag_sm, extent = c(0, 1, 0, 1), scale = 1, worker = 100))
})


test_that("Check with elev_", {
  tag_tmp <- trainset_read(tag,
    directory = system.file("extdata/1_pressure/labels/", package = "GeoPressureR"),
    filename = "18LX_act_pres-labeled_elev.csv"
  )
  tag_tmp <- tag_stap(tag_tmp)
  mismatch <- geopressure_mismatch(tag_tmp,
    extent = c(-16, 23, 0, 50),
    scale = 1,
  )
  expect_length(mismatch, 5)
})


test_that("Check geopressure_likelihood() output", {
  expect_error(geopressure_likelihood(pressure_mismatch, s = "not_a_number"))
  expect_error(geopressure_likelihood(pressure_mismatch, thr = "not_a_number"))
  expect_error(geopressure_likelihood(pressure_mismatch, fun_w = "not_a_function"))
})


test_that("Check map2path() output", {
  # Error on incorrect parameter
  expect_error(map2path(pressure_likelihood), NA)
  expect_warning(map2path(pressure_likelihood, interp = 2), "*First and last stap are shorter*")
  expect_error(map2path(pressure_likelihood, format = "ind"), NA)
  expect_error(map2path(pressure_likelihood, format = "arr.ind"), NA)

  # Check correct returned
  expect_s3_class(path_pressure, "data.frame")
  expect_true(all(c("lon", "lat", "stap") %in% names(path_pressure)))
  expect_gt(nrow(path_pressure), 0)
})


test_that("Check for incomplete stap", {
  tag_tmp <- tag
  tag_tmp$pressure <- subset(tag_tmp$pressure, stap == 3 | stap == 4)

  # Check geopressure_mismatch
  expect_warning(mismatch <- geopressure_mismatch(tag_tmp,
    extent = c(-16, 23, 0, 50),
    scale = 1,
  ))
  expect_true(length(mismatch) == nrow(tag$stap))
  expect_equal(length(dim(mismatch[[3]]$mse)), 2)

  # Check geopressure_likelihood
  likelihood <- geopressure_likelihood(mismatch)
  expect_true(length(likelihood) == nrow(tag$stap))
  expect_equal(length(dim(likelihood[[4]]$likelihood)), 2)

  # Check map2path
  expect_error(map2path(likelihood), NA)
  expect_error(map2path(likelihood, interp = 0.2), NA)
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

  pressure_timeserie <- geopressure_timeseries(lon = 6, lat = 46, pressure = tag_sm$pressure)
  expect_s3_class(pressure_timeserie, "data.frame")
  expect_true(all(c("date", "pressure") %in% names(pressure_timeserie)))

  i_s <- 4
  n <- c(16, 32, 10) # number of valid datapoint for this stap
  path <- subset(path_pressure, stap == i_s)
  #
  # # Test Include flight
  pressure <- subset(tag$pressure, stap == i_s)
  pressure_timeserie <- geopressure_timeseries(path$lon, path$lat, pressure)
  expect_true(all(c("date", "pressure", "altitude", "pressure0", "stap")
  %in% names(pressure_timeserie)))
  expect_equal(nrow(pressure_timeserie), n[2])

  pressure <- subset(tag$pressure, stap == i_s | stap == 0)
  pressure_timeserie <- geopressure_timeseries(path$lon, path$lat, pressure)
  expect_gt(nrow(pressure_timeserie), n[2])
})



test_that("Check geopressure_timeseries_path() output", {
  # Check for incorrect input
  expect_error(geopressure_timeseries_path(path_pressure, "no_a_pressure"))
  expect_error(geopressure_timeseries_path("not_a_path", tag$pressure))
  expect_error(geopressure_timeseries_path(path_pressure, tag$pressure, include_flight = 2))
  expect_error(geopressure_timeseries_path(path_pressure, tag$pressure, include_flight = NA))
  expect_error(geopressure_timeseries_path(path_pressure, tag$pressure, include_flight = c(0, 10)))

  i_s <- 4
  n <- c(16, 32, 10)
  pressure <- subset(tag$pressure, stap == i_s)
  path <- subset(path_pressure, stap == i_s)
  pressure_timeserie <- geopressure_timeseries_path(path, pressure)

  # Test Include flight
  pressure <- tag$pressure
  pressure_timeserie <- geopressure_timeseries_path(path, pressure)
  expect_equal(nrow(pressure_timeserie[[1]]), n[2])
  pressure_timeserie <- geopressure_timeseries_path(path, pressure, include_flight = TRUE)
  expect_equal(nrow(pressure_timeserie[[1]]), sum(n))
  expect_equal(sum(pressure_timeserie[[1]]$stap == i_s), n[2])
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

  expect_warning(geopressure_timeseries_path(path, pressure))
})
