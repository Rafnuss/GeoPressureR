library(testthat)
library(GeoPressureR)

pressure_mismatch <- readRDS(
  system.file(
    "extdata/1_pressure/18LX_pressure_mismatch.rds",
    package = "GeoPressureR"
  )
)
pressure_likelihood <- geopressure_likelihood(pressure_mismatch)

test_that("Check map2path() output", {
  # Error on incorrect parameter
  expect_no_error(path_pressure <- map2path(pressure_likelihood))
  expect_warning(map2path(pressure_likelihood, interp = 2), "*First and last stap are shorter*")
  expect_no_error(map2path(pressure_likelihood, format = "ind"))
  expect_no_error(map2path(pressure_likelihood, format = "arr.ind"))

  # Check correct returned
  expect_s3_class(path_pressure, "data.frame")
  expect_true(all(c("lon", "lat", "stap") %in% names(path_pressure)))
  expect_gt(nrow(path_pressure), 0)
})
