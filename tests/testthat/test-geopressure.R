library(testthat)
library(GeoPressureR)

# Hide cli message
# options(cli.default_handler = function(...) { })

# Set working directory
# setwd(system.file("extdata/", package = "GeoPressureR"))

# Small synthetic case
pressure <- data.frame(
  date = as.POSIXct(c(
    "2017-06-20 00:00:00 UTC", "2017-06-20 01:00:00 UTC",
    "2017-06-20 02:00:00 UTC", "2017-06-20 03:00:00 UTC"
  ), tz = "UTC"),
  value = c(1000, 1000, 1000, 1000),
  label = c("", "", "", ""),
  stap_id = c(1, 1, 1, 1)
)
stap <- data.frame(
  stap_id = 1,
  start = "2017-06-20 00:00:00 UTC",
  end = "2017-06-20 03:00:00 UTC"
)
tag <- list(
  id = "18LX",
  stap = stap,
  pressure = pressure
)
extent <- c(0, 1, 0, 1)
scale <- 1
known <- data.frame(
  stap_id = 1,
  known_lon = .5,
  known_lat = .9
)

tag <- tag_create(tag, extent, scale)

test_that("tag_create() | default output", {
  expect_true(assertthat::has_name(tag, c("id", "stap", "scale", "extent")))
  expect_true(all(tag$stap$include))
  expect_true(all(is.na(tag$stap$lon)))
  expect_no_error(tag <- tag_create(tag, extent, known = known))
  expect_true(!is.na(tag$stap$known_lon[1]))
  expect_error(tag <- tag_create(tag, extent, known = data.frame(
    stap_id = 1,
    known_lon = 2,
    known_lat = .9
  )), "*The known latitude and longitude*")
})

tag <- geopressure_map_mismatch(tag, pressure)

test_that("geopressure_map_mismatch() | default output", {
  expect_type(tag, "list")
  expect_equal(length(dim(tag$mse[[1]])), 2)
  expect_equal(length(dim(tag$mask[[1]])), 2)
  expect_true(tag$stap$nb_sample == 4)
})

test_that("geopressure_map_mismatch() | timeout and worker", {
  expect_error(geopressure_map_mismatch(tag, pressure, timeout = 0.1), "*Timeout was reached*")
  expect_error(geopressure_map_mismatch(tag, pressure, worker = 100), "* workers < 100*")
})


tag <- geopressure_map_likelihood(tag)

test_that("geopressure_map_likelihood() | default output", {
  expect_equal(length(dim(tag$map_pressure[[1]])), 2)

  expect_error(geopressure_map_likelihood(tag, s = "not_a_number"))
  expect_error(geopressure_map_likelihood(tag, thr = "not_a_number"))
  expect_error(geopressure_map_likelihood(tag, log_linear_pooling_weight = "not_a_function"))
})


test_that("geopressure_map() | default output", {
  tag <- tag_create(tag, extent, scale)
  expect_no_error(tag <- geopressure_map(tag, tag$pressure))
  expect_true(assertthat::has_name(tag, c("id", "stap", "likelihood", "param", "mask_water")))
})
