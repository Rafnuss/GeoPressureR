library(testthat)
library(GeoPressureR)

# Set working directory
setwd(system.file("extdata", package = "GeoPressureR"))

test_that("tag_create() | manufacturer", {
  expect_no_error(tag_create(id = "18LX", quiet = TRUE))
  expect_no_error(tag_create(id = "CB621", quiet = TRUE))
  expect_no_error(tag_create(id = "X19D", quiet = TRUE))
  expect_no_error(tag_create(
    id = "C003654",
    manufacturer = "prestag",
    quiet = TRUE
  ))

  pres <- data.frame(
    date = as.POSIXct(
      c(
        "2017-06-20 00:00:00 UTC",
        "2017-06-20 01:00:00 UTC",
        "2017-06-20 02:00:00 UTC",
        "2017-06-20 03:00:00 UTC"
      ),
      tz = "UTC"
    ),
    value = c(1000, 1000, 1000, 1000)
  )
  expect_no_error(tag_create(id = "dummy", pressure_file = pres, quiet = TRUE))
})

test_that("tag_create() | default", {
  # with crop
  tag <- tag_create(
    id = "18LX",
    crop_start = "2017-06-20",
    crop_end = "2018-05-02",
    quiet = TRUE
  )

  # Check that the return tag is correct
  expect_no_error(tag_assert(tag, "pressure"))
  expect_no_error(tag_assert(tag, "light"))
  expect_no_error(tag_assert(tag, "acceleration"))

  expect_gt(nrow(tag$pressure), 0)
  expect_gt(nrow(tag$light), 0)
  expect_gt(nrow(tag$acceleration), 0)

  expect_error(expect_warning(tag_create(
    id = "18LX",
    pressure_file = "wrong_file"
  )))
  expect_warning(tag_create(id = "18LX", light_file = "wrong_file"))

  # Check crop
  expect_warning(expect_warning(expect_warning(expect_warning(tag_create(
    id = "18LX",
    crop_start = "2019-06-20",
    crop_end = "2018-05-02"
  )))))
})

test_that("tag_create() | Migrate Technology", {
  tag <- tag_create(
    id = "CB621",
    pressure_file = "*.deg",
    light_file = "*.lux",
    acceleration_file = NA,
    quiet = TRUE
    # crop_start = "2017-06-20", crop_end = "2018-05-02"
  )
  expect_gt(nrow(tag$pressure), 0)
  expect_gt(nrow(tag$light), 0)

  tag <- tag_create(
    id = "CB621",
    acceleration_file = NA,
    quiet = TRUE
    # crop_start = "2017-06-20", crop_end = "2018-05-02"
  )
})


test_that("tag_create() | no acceleration", {
  expect_no_error(
    tag <- tag_create(
      id = "18LX",
      acceleration_file = NA,
      light_file = NA,
      quiet = TRUE
    )
  )
  expect_no_error(
    tag <- tag_label_read(
      tag,
      file = "./data/tag-label/18LX-labeled-no_acc.csv"
    )
  )
  expect_no_error(tag_label_stap(tag, quiet = TRUE))
})

test_that("param_create() | default", {
  expect_no_error(param_create(id = "18LX", extent = c(0, 0, 1, 1)))
  expect_no_error(param_create(id = "18LX", default = TRUE))
})
