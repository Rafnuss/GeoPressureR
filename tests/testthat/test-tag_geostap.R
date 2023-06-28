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
  stap_id = c(1, 1, 2, 2)
)
stap <- data.frame(
  stap_id = c(1, 2),
  start = c("2017-06-20 00:00:00 UTC", "2017-06-21 0:00:00 UTC"),
  end = c("2017-06-20 03:00:00 UTC", "2017-06-23 00:00:00 UTC")
)
extent <- c(0, 1, 0, 1)
scale <- 1
known <- data.frame(
  stap_id = 1,
  known_lon = .5,
  known_lat = .9
)

tag <- structure(list(
  id = "18LX",
  stap = stap,
  pressure = pressure
), class = "tag")



test_that("tag_geostap() | no known", {
  tag_no_known <- tag_geostap(tag, extent, scale)
  expect_true(assertthat::has_name(tag_no_known, c("id", "stap", "scale", "extent")))
  expect_true(all(tag_no_known$stap$include))
  expect_true(all(is.na(tag_no_known$stap$known_lat)))
})

test_that("tag_geostap() | with known", {
  tag_with_known <- tag_geostap(tag, extent, scale, known = known)
  expect_equal(tag_with_known$stap$known_lon[1], known$known_lon)
  known_overwrite <- data.frame(
    stap_id = 1,
    known_lon = .8,
    known_lat = .9
  )
  expect_warning(tag_overwrite <- tag_geostap(tag_with_known, extent, known = known_overwrite), "*The known latitude and longitude*")
  expect_equal(tag_overwrite$stap$known_lon[1], known_overwrite$known_lon)
})

test_that("tag_geostap() | with include", {
  expect_error(tag_geostap(tag, extent, include_stap_id = 99))

  expect_warning(tag_include <- tag_geostap(tag, extent, include_stap_id = 2))
  expect_equal(tag_include$stap$include, c(F,T))

  expect_warning(tag_include <- tag_geostap(tag, extent, include_min_duration = 12))
  expect_equal(tag_include$stap$include, c(F,T))
})
