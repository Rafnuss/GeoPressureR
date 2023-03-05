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

geostap <- geostap_create(tag, extent, scale)

test_that("geostap_create() | default output", {
  expect_true(assertthat::has_name(geostap, c("id", "stap", "scale", "extent")))
  expect_true(all(geostap$stap$include))
  expect_true(all(is.na(geostap$stap$lon)))
  expect_no_error(geostap <- geostap_create(tag, extent, known = known))
  expect_true(!is.na(geostap$stap$known_lon[1]))
  expect_error(geostap <- geostap_create(tag, extent, known = data.frame(
    stap_id = 1,
    known_lon = 2,
    known_lat = .9
  )), "*The known latitude and longitude*")
})

geostap <- geopressure_map_mismatch(geostap, pressure)

test_that("geopressure_map_mismatch() | default output", {
  expect_type(geostap, "list")
  expect_equal(length(dim(geostap$mse[[1]])), 2)
  expect_equal(length(dim(geostap$mask[[1]])), 2)
  expect_true(geostap$stap$nb_sample == 4)
})

test_that("geopressure_map_mismatch() | timeout and worker", {
  expect_error(geopressure_map_mismatch(geostap, pressure, timeout = 0.1), "*Timeout was reached*")
  expect_error(geopressure_map_mismatch(geostap, pressure, worker = 100), "* workers < 100*")
})


geostap <- geopressure_map_likelihood(geostap)

test_that("geopressure_map_likelihood() | default output", {
  expect_equal(length(dim(geostap$map_pressure[[1]])), 2)

  expect_error(geopressure_map_likelihood(geostap, s = "not_a_number"))
  expect_error(geopressure_map_likelihood(geostap, thr = "not_a_number"))
  expect_error(geopressure_map_likelihood(geostap, log_linear_pooling_weight = "not_a_function"))
})


test_that("geopressure_map() | default output", {
  geostap <- geostap_create(tag, extent, scale)
  expect_no_error(geostap <- geopressure_map(geostap, tag$pressure))
  expect_true(assertthat::has_name(geostap, c("id", "stap", "likelihood", "param", "mask_water")))
})
