library(testthat)
library(GeoPressureR)

# Hide cli message
options(cli.default_handler = function(...) { })

# Set working directory
setwd(system.file("extdata/", package = "GeoPressureR"))


tag <- tag_read(
  id = "18LX",
  crop_start = "2017-06-20", crop_end = "2018-05-02"
)

test_that("tag_read() | default", {
  # Check for error with incorrect input
  expect_error(tag_read(id = 23))
  expect_error(tag_read(id = "18LX", file = "not a file"))

  # Check that the return tag is correct
  expect_type(tag, "list")
  expect_true(all(c("pressure", "light", "acceleration") %in% names(tag)))
  expect_gt(nrow(tag$pressure), 0)
  expect_gt(nrow(tag$light), 0)
  expect_gt(nrow(tag$acceleration), 0)

  expect_error(expect_warning(tag_read("18LX",
    pressure_file = "wrong_file"
  )))

  # Check crop
  expect_true(nrow(tag_read("18LX",
    crop_start = "2019-06-20", crop_end = "2018-05-02"
  )$pressure) == 0)
})

test_that("tag_read() | Migrate Technology", {
  tag <- tag_read("CB621",
    pressure_file = ".deg",
    light_file = ".lux",
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
    file = "data/1-tag_label/18LX-labeled-no_acc.csv"
  ))
  expect_no_error(tag_label_stap(tag))
})
