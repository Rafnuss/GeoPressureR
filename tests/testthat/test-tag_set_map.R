library(testthat)
library(GeoPressureR)

# Hide cli message
# options(cli.default_handler = function(...) { })

# Set working directory
# setwd(system.file("extdata", package = "GeoPressureR"))

# Small synthetic case
pressure <- data.frame(
  date = as.POSIXct(
    c(
      "2017-06-20 00:00:00 UTC",
      "2017-06-20 01:00:00 UTC",
      "2017-06-20 02:00:00 UTC",
      "2017-06-20 03:00:00 UTC"
    ),
    tz = "UTC"
  ),
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

tag <- structure(
  list(
    param = param_create(id = "18LX"),
    stap = stap,
    pressure = pressure
  ),
  class = "tag"
)


test_that("tag_set_map() | no known", {
  tag_no_known <- tag_set_map(tag, extent, scale)
  expect_true(assertthat::has_name(
    tag_no_known$param$tag_set_map,
    c("scale", "extent")
  ))
  expect_true(all(tag_no_known$stap$include))
  expect_true(all(is.na(tag_no_known$stap$known_lat)))
})

test_that("tag_set_map() | with known", {
  tag_with_known <- tag_set_map(tag, extent, scale, known = known)
  expect_equal(tag_with_known$stap$known_lon[1], known$known_lon)
  known_overwrite <- data.frame(
    stap_id = 1,
    known_lon = .8,
    known_lat = .9
  )
  expect_warning(
    tag_overwrite <- tag_set_map(
      tag_with_known,
      extent,
      known = known_overwrite
    )
  )
  expect_equal(tag_overwrite$stap$known_lon[1], known_overwrite$known_lon)
})

test_that("tag_set_map() | with include", {
  expect_error(tag_set_map(tag, extent, include_stap_id = 99))

  tag_include <- tag_set_map(tag, extent, include_stap_id = 2)
  expect_equal(tag_include$stap$include, c(FALSE, TRUE))

  tag_include <- tag_set_map(tag, extent, include_min_duration = 12)
  expect_equal(tag_include$stap$include, c(FALSE, TRUE))
})
