library(testthat)
library(GeoPressureR)

# Set working directory
# setwd(system.file("extdata", package = "GeoPressureR"))

data <- lapply(1:10, \(x) matrix(runif(5000), nrow = 50, ncol = 100))
scale <- 10
extent <- c(0, 10, 0, 5)
seq(as.Date("2023-01-01"), as.Date("2023-01-10"), by = "day")
stap <- data.frame(
  stap_id = 1:10,
  start = seq(
    as.POSIXct("2023-01-01", tz = "UTC"),
    as.POSIXct("2023-01-10 UTC", tz = "UTC"),
    by = "day"
  )
)
stap$end <- stap$start + sample(1:10) * 10000

test_that("map_create() | basic", {
  expect_error(map_create())
  expect_no_error(
    map <- map_create(
      data = data,
      extent = extent,
      scale = scale,
      stap = stap,
      id = "18LX",
      type = "pressure"
    )
  )

  expect_no_error(map_create(
    data = data,
    extent = extent,
    scale = scale,
    stap = stap
  ))

  expect_type(map[1], "list")
  expect_true(inherits(map[[1]], "matrix"))
  expect_equal(dim(map), c(50, 100, 10))
  expect_equal(length(map), 10)
  expect_no_error(map$lat)
  expect_equal(map$stap, stap)
  expect_equal(map$scale, scale)
  expect_equal(map$extent, extent)

  expect_no_error(plot(map))
  expect_no_error(print(map))
})


test_that("map_create() | missing stap", {
  data_tmp <- data
  data_tmp[1] <- vector("list", 1)
  expect_no_error(
    map <- map_create(
      data = data_tmp,
      extent = extent,
      scale = scale,
      stap = stap
    )
  )
})


test_that("rast.map() | ", {
  map <- map_create(
    data = data,
    extent = extent,
    scale = scale,
    stap = stap,
    id = "18LX",
    type = "pressure"
  )

  expect_no_error(r <- rast.map(map))
  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::time(r), stap$start)
})
