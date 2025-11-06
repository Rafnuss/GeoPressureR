library(testthat)
library(GeoPressureR)

# Set working directory
setwd(system.file("extdata", package = "GeoPressureR"))

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
  stap_id = c(1, 1, 1, 1)
)
stap <- data.frame(
  stap_id = 1,
  start = "2017-06-20 00:00:00 UTC",
  end = "2017-06-20 03:00:00 UTC"
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
    param = param_create("18LX", quiet = TRUE),
    stap = stap,
    pressure = pressure
  ),
  class = "tag"
)

tag <- tag_set_map(tag, extent, scale = scale)

tag <- geopressure_map_mismatch(tag, quiet = TRUE)


test_that("geopressure_map_mismatch() | default output", {
  expect_type(tag, "list")
  expect_equal(length(dim(tag$map_pressure_mse[[1]])), 2)
  expect_equal(length(dim(tag$map_pressure_mask[[1]])), 2)
  expect_s3_class(tag$map_pressure_mask, "map")
  expect_true(tag$stap$nb_sample == 4)
})

test_that("geopressure_map_mismatch() | timeout and worker", {
  expect_error(
    geopressure_map_mismatch(tag, timeout = 0.001, quiet = TRUE),
    "*Timeout was reached*"
  )
  expect_error(
    geopressure_map_mismatch(tag, worker = 100, quiet = TRUE),
    "* workers < 100*"
  )
})


test_that("geopressure_map_mismatch() | date too early", {
  pressure <- data.frame(
    date = as.POSIXct(
      c(
        "2017-06-20 00:00:00 UTC",
        "2017-06-20 01:00:00 UTC",
        "2017-06-20 02:00:00 UTC",
        "2017-06-20 03:00:00 UTC",
        "2037-06-20 00:00:00 UTC",
        "2037-06-20 01:00:00 UTC",
        "2037-06-20 02:00:00 UTC",
        "2037-06-20 03:00:00 UTC"
      ),
      tz = "UTC"
    ),
    value = c(1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000),
    label = c("", "", "", ""),
    stap_id = c(1, 1, 1, 1, 2, 2, 2, 2)
  )
  stap <- data.frame(
    stap_id = c(1, 2),
    start = c("2017-06-20 00:00:00 UTC", "2037-06-20 00:00:00 UTC"),
    end = c("2017-06-20 03:00:00 UTC", "2037-06-20 03:00:00 UTC")
  )

  tag <- structure(
    list(
      param = param_create("18LX"),
      stap = stap,
      pressure = pressure
    ),
    class = "tag"
  )

  tag <- tag_set_map(tag, extent, scale = scale)

  expect_error(
    # fail for after date
    expect_warning(
      # warning after date
      expect_warning(
        # irregular
        tag <- geopressure_map_mismatch(tag, quiet = TRUE)
      )
    )
  )
})

tag <- geopressure_map_likelihood(tag)

test_that("geopressure_map_likelihood() | default output", {
  expect_equal(tag$map_pressure$id, "18LX")
  expect_equal(tag$map_pressure$extent, extent)
  expect_equal(tag$map_pressure$scale, scale)
  expect_equal(tag$map_pressure$type, "pressure")
  expect_s3_class(tag$map_pressure, "map")
  expect_equal(length(dim(tag$map_pressure[[1]])), 2)

  expect_error(geopressure_map_likelihood(tag, s = "not_a_number"))
  expect_error(geopressure_map_likelihood(
    tag,
    log_linear_pooling_weight = "not_a_function"
  ))
})


test_that("geopressure_map() | default output", {
  tag <- tag_set_map(tag, extent, scale)
  expect_no_error(tag <- geopressure_map(tag, quiet = TRUE))
  expect_true(assertthat::has_name(tag, c("stap", "map_pressure", "param")))
  expect_true(assertthat::has_name(
    tag$map_pressure,
    c(
      "id",
      "stap",
      "data",
      "extent",
      "scale",
      "lat",
      "lon",
      "type",
      "mask_water"
    )
  ))
})
