library(testthat)
library(GeoPressureR)

pathname <- system.file("extdata/0_PAM/18LX", package = "GeoPressureR")

pam <- pam_read(
  pathname = pathname,
  crop_start = "2017-06-20", crop_end = "2018-05-02"
)

test_that("Check pam_read()", {
  # Check for error with incorrect input
  expect_error(pam_read("not a path"))
  expect_type(pam_read(pathname = pathname), "list")
  expect_error(pam_read("not a path"))

  # Check that the return pam is correct
  expect_type(pam, "list")
  expect_true(all(c("pressure", "light", "acceleration") %in% names(pam)))
  expect_gt(nrow(pam$pressure), 0)
  expect_gt(nrow(pam$light), 0)
  expect_gt(nrow(pam$acceleration), 0)

  expect_warning(pam_read(
    pathname = pathname,
    pressure_file = "wrong_file"
  ))

  # Check crop
  expect_true(nrow(pam_read(
    pathname = pathname,
    crop_start = "2019-06-20", crop_end = "2018-05-02"
  )$light) == 0)
})

test_that("Check pam_read() for Migrate Technology", {
  pathname <- system.file("extdata/0_PAM/CB621", package = "GeoPressureR")

  pam <- pam_read(
    pathname = pathname,
    pressure_file = ".deg",
    light_file = ".lux",
    acceleration_file = NA
    # crop_start = "2017-06-20", crop_end = "2018-05-02"
  )
  expect_gt(nrow(pam$pressure), 0)
  expect_gt(nrow(pam$light), 0)
})

pam_classified <- pam_classify(pam)
test_that("Check pam_classify()", {
  expect_error(pam_classify("not a pam"))
  expect_true(c("ismig") %in% names(pam_classified$acceleration))
  expect_type(pam_classified$acceleration$ismig, "logical")
})


test_that("Check trainset_write()", {
  expect_error(trainset_write("not a pam", pathname = tempdir()))
  # Work under normal condition
  expect_error(trainset_write(pam_classified, pathname = tempdir()), NA)
  # Work even if not auto-classified
  expect_error(trainset_write(pam, pathname = tempdir()), NA)

  # create new folder
  expect_error(trainset_write(pam, pathname = paste0(tempdir(), "/", Sys.Date())), NA)

  # Check with outlier
  pam$pressure$isoutlier <- TRUE
  expect_error(trainset_write(pam, pathname = tempdir()), NA)

  # Check for back compatibility (isoutliar instead of isoutlier) outlier
  names(pam$pressure)[3] <- "isoutliar"
  expect_warning(trainset_write(pam, pathname = tempdir()))
})

pathname <- system.file("extdata/1_pressure/labels", package = "GeoPressureR")

pam_labeled <- trainset_read(pam, pathname = pathname)
test_that("Check trainset_read()", {
  # Returned value is correct
  expect_type(pam_labeled, "list")
  expect_true(c("isoutlier") %in% names(pam_labeled$pressure))
  expect_type(pam_labeled$pressure$isoutlier, "logical")
  expect_true(c("ismig") %in% names(pam_labeled$acceleration))
  expect_type(pam_labeled$acceleration$ismig, "logical")

  # Return error for incorrect input
  expect_error(trainset_read(pam, pathname = "not a path"))
  expect_error(trainset_read("not a pam", pathname = pathname))

  # Test with different labeled file size
  expect_warning(pam <- trainset_read(pam,
    pathname = pathname,
    filename = "18LX_act_pres-labeled-diffsize.csv"
  ))
  expect_true(c("isoutlier") %in% names(pam$pressure))
  expect_true(c("ismig") %in% names(pam$acceleration))
})


pam_sta <- pam_sta(pam_labeled)
test_that("Check pam_sta()", {
  # Returned value is correct
  expect_type(pam_sta, "list")
  expect_true(c("sta") %in% names(pam_sta))
  expect_type(pam_labeled$pressure$isoutlier, "logical")
  expect_gt(nrow(pam_sta$sta), 0)
})
