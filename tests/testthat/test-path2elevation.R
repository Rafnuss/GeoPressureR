library(testthat)
library(GeoPressureR)

# Small synthetic case
path <- data.frame(
  lon = c(8.47, 9.41, 9.01, -0.91, 14.24, 27.30, 34.39, 30.00),
  lat = c(48.89, 44.78, 40.07, 37.68, 17.33, 7.32, 8.09, -23.13),
  start = as.POSIXct(
    c(
      "2017-05-01 00:42", "2017-05-03 01:22", "2017-05-03 22:47", "2017-05-06 03:32",
      "2017-05-07 01:12", "2017-05-07 22:32", "2017-05-09 21:52", "2017-05-10 21:12"
    ),
    tz = "UTC"
  ),
  end = as.POSIXct(
    c(
      "2017-05-02 22:12", "2017-05-03 20:12", "2017-05-06 02:47", "2017-05-06 23:22",
      "2017-05-07 17:42", "2017-05-09 20:27", "2017-05-10 19:57", "2017-05-11 21:17"
    ),
    tz = "UTC"
  ),
  stap_id = seq(1, 8)
)

test_that("path2elevation() | default output", {
  elevation <- path2elevation(path)
  expect_s3_class(elevation, "data.frame")
  expect_true(all(
    c("X10", "X50", "X90", "distance", "lat", "lon", "stap_id") %in% names(elevation)
  ))
})

# test_that("path2elevation() | change default", {
#   elevation <- path2elevation(path, scale = 1)
#   elevation <- path2elevation(path, sampling_scale = 1)
# })
