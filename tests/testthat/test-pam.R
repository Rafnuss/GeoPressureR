library(testthat)
library(GeoPressureR)

pathname <- system.file("extdata", package = "GeoPressureR")


pam_data <- pam_read(
  pathname = pathname,
  crop_start = "2017-06-20", crop_end = "2018-05-02"
)
test_that("Check pam_read()", {
  expect_error(pam_read("not a path"))
  expect_type(pam_read(pathname = pathname), "list")
  expect_type(pam_data, "list")
  expect_true(all(c("pressure", "light", "acceleration") %in% names(pam_data)))
  expect_gt(nrow(pam_data$pressure), 0)
  expect_gt(nrow(pam_data$light), 0)
  expect_gt(nrow(pam_data$acceleration), 0)
  expect_true(nrow(pam_read(
    pathname = pathname,
    crop_start = "2019-06-20", crop_end = "2018-05-02"
  )$light) == 0)
  # Test extension
})

pam_data_classified <- pam_classify(pam_data)
test_that("Check pam_classify()", {
  expect_error(pam_classify("not a pam"))
  expect_true(c("ismig") %in% names(pam_data_classified$acceleration))
  expect_type(pam_data_classified$acceleration$ismig, "logical")
})


pam_data_labeled <- trainset_read(pam_data, pathname = pathname)
test_that("Check trainset_read()", {
  # Returned value is correct
  expect_type(pam_data_labeled, "list")
  expect_true(c("isoutliar") %in% names(pam_data_labeled$pressure))
  expect_type(pam_data_labeled$pressure$isoutliar, "logical")
  expect_true(c("ismig") %in% names(pam_data_labeled$acceleration))
  expect_type(pam_data_labeled$acceleration$ismig, "logical")

  # Return error for incorrect input
  expect_error(trainset_read(pam_data, pathname = "not a path"))
  expect_error(trainset_read("not a pam", pathname = pathname))
})


pam_data_sta <- pam_sta(pam_data_labeled)
test_that("Check pam_sta()", {
  # Returned value is correct
  expect_type(pam_data_sta, "list")
  expect_true(c("sta") %in% names(pam_data_sta))
  expect_type(pam_data_labeled$pressure$isoutliar, "logical")
  expect_gt(nrow(pam_data_sta$sta), 0)
})
