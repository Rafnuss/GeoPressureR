pressure <- data.frame(
  date = as.POSIXct(c(
    "2017-06-20 00:00:00 UTC", "2017-06-20 01:00:00 UTC",
    "2017-06-20 02:00:00 UTC"
  )),
  obs = c(1000, 1000, 1000),
  sta_id = c(1, 1, 1)
)
raster_list <- geopressure_map(pressure, extent = c(1, 0, 0, 1), scale = 1)

test_that("Check geopressure_map() output", {
  expect_type(raster_list, "list")
  expect_length(raster_list, 1)
  expect_s4_class(raster_list[[1]], "RasterBrick")
})



pressure_timeserie <- geopressure_ts(
  lon = 6, lat = 46,
  start_time = as.POSIXct("2017-06-20 00:00:00 UTC"),
  end_time = as.POSIXct("2017-06-20 02:00:00 UTC")
)
test_that("Check geopressure_ts(lon,lat,start_time, end_time) output", {
  expect_s3_class(pressure_timeserie, "data.frame")
})



pressure_timeserie <- geopressure_ts(lon = 6, lat = 46, pressure = pressure)
test_that("Check geopressure_ts(lon,lat,pressure) output", {
  expect_s3_class(pressure_timeserie, "data.frame")
})
