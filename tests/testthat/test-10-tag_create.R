library(testthat)
library(GeoPressureR)

# Hide cli message
options(cli.default_handler = function(...) { })

# Set working directory
setwd(system.file("extdata/", package = "GeoPressureR"))


tag <- tag_create(
  id = "18LX",
  crop_start = "2017-06-20", crop_end = "2018-05-02"
)

test_that("tag_create() | default", {
  # Check id
  expect_error(tag_create(id = 23))
  expect_no_error(tag_assert(tag_create("18LX")))
  expect_no_error(tag_create(id = "18LX", extent = c(2,3,4,1)))
  expect_no_error(tag_assert(tag))
})

test_that("tag_read() | default", {
  # check if id and tag are mixed up
  tag <- tag_create("18LX")
  expect_no_error(tag_read("18LX"))
  expect_error(tag_read(tag = "18LX"))
  expect_error(tag_read(id = tag))
  expect_error(tag_read(tag))
  expect_no_error(tag_read(tag = tag))
  expect_no_error(tag_read(id = "18LX"))

  # with crop
  tag <- tag_read(id = "18LX", crop_start = "2017-06-20", crop_end = "2018-05-02")

  # Check that the return tag is correct
  expect_no_error(tag_assert(tag, "read"))
  expect_no_error(tag_assert(tag, "pressure"))
  expect_no_error(tag_assert(tag, "light"))
  expect_no_error(tag_assert(tag, "acceleration"))

  expect_gt(nrow(tag$pressure), 0)
  expect_gt(nrow(tag$light), 0)
  expect_gt(nrow(tag$acceleration), 0)

  expect_error(expect_warning(tag_read(id = "18LX",pressure_file = "wrong_file")))
  expect_warning(tag_read(id = "18LX", light_file = "wrong_file"))

  # Check crop
  expect_true(nrow(tag_read(id = "18LX",
    crop_start = "2019-06-20", crop_end = "2018-05-02"
  )$pressure) == 0)
})

test_that("tag_create() | Migrate Technology", {
  tag <- tag_read(id = "CB621",
    pressure_file = "*.deg",
    light_file = "*.lux",
    acceleration_file = NA
    # crop_start = "2017-06-20", crop_end = "2018-05-02"
  )
  expect_gt(nrow(tag$pressure), 0)
  expect_gt(nrow(tag$light), 0)
})


test_that("tag_read() | no acceleration", {
  expect_no_error(tag <- tag_read(
    id = "18LX",
    acceleration_file = NA,
    light_file = NA
  ))
  expect_no_error(tag <- tag_label_read(tag,
    file = "./data/tag-label/18LX-labeled-no_acc.csv"
  ))
  expect_no_error(tag_label_stap(tag))
})
