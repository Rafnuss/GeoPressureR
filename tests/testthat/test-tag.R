library(testthat)
library(GeoPressureR)

pathname <- system.file("extdata/0_tag/18LX", package = "GeoPressureR")

tag <- tag_read(
  pathname = pathname,
  crop_start = "2017-06-20", crop_end = "2018-05-02"
)

test_that("Check tag_read()", {
  # Check for error with incorrect input
  expect_error(tag_read("not a path"))
  expect_type(tag_read(pathname = pathname), "list")
  expect_error(tag_read("not a path"))

  # Check that the return tag is correct
  expect_type(tag, "list")
  expect_true(all(c("pressure", "light", "acceleration") %in% names(tag)))
  expect_gt(nrow(tag$pressure), 0)
  expect_gt(nrow(tag$light), 0)
  expect_gt(nrow(tag$acceleration), 0)

  expect_warning(tag_read(
    pathname = pathname,
    pressure_file = "wrong_file"
  ))

  # Check crop
  expect_true(nrow(tag_read(
    pathname = pathname,
    crop_start = "2019-06-20", crop_end = "2018-05-02"
  )$light) == 0)
})

test_that("Check tag_read() for Migrate Technology", {
  pathname <- system.file("extdata/0_tag/CB621", package = "GeoPressureR")

  tag <- tag_read(
    pathname = pathname,
    pressure_file = ".deg",
    light_file = ".lux",
    acceleration_file = NA
    # crop_start = "2017-06-20", crop_end = "2018-05-02"
  )
  expect_gt(nrow(tag$pressure), 0)
  expect_gt(nrow(tag$light), 0)
})

tag_classified <- tag_classify(tag)
test_that("Check tag_classify()", {
  expect_error(tag_classify("not a tag"))
  expect_true(c("ismig") %in% names(tag_classified$acceleration))
  expect_type(tag_classified$acceleration$ismig, "logical")
})


test_that("Check trainset_write()", {
  expect_error(trainset_write("not a tag", pathname = tempdir()))
  # Work under normal condition
  expect_error(trainset_write(tag_classified, pathname = tempdir()), NA)
  # Work even if not auto-classified
  expect_error(trainset_write(tag, pathname = tempdir()), NA)

  # create new folder
  expect_error(trainset_write(tag, pathname = paste0(tempdir(), "/", Sys.Date())), NA)

  # Check with outlier
  # tag$pressure$isoutlier <- TRUE
  # expect_error(trainset_write(tag, pathname = tempdir()), NA)
})

pathname <- system.file("extdata/1_pressure/labels", package = "GeoPressureR")

tag_labeled <- trainset_read(tag, pathname = pathname)
test_that("Check trainset_read()", {
  # Returned value is correct
  expect_type(tag_labeled, "list")
  expect_true(c("label") %in% names(tag_labeled$pressure))
  expect_type(tag_labeled$pressure$label, "character")
  expect_true(c("label") %in% names(tag_labeled$acceleration))
  expect_type(tag_labeled$acceleration$label, "character")

  # Return error for incorrect input
  expect_error(trainset_read(tag, pathname = "not a path"))
  expect_error(trainset_read("not a tag", pathname = pathname))

  # Test with different labeled file size
  expect_warning(tag <- trainset_read(tag,
    pathname = pathname,
    filename = "18LX_act_pres-labeled-diffsize.csv"
  ))
})


tag_stap <- tag_stap(tag_labeled)
test_that("Check tag_stap()", {
  # Returned value is correct
  expect_type(tag_stap, "list")
  expect_true(c("stap") %in% names(tag_stap))
  expect_type(tag_labeled$pressure$label, "character")
  expect_type(tag_labeled$acceleration$label, "character")
  expect_gt(nrow(tag_stap$stap), 0)
})



test_that("for elev", {
  tag_labeled <- trainset_read(tag,
    pathname = system.file("extdata/1_pressure/labels/", package = "GeoPressureR"),
    filename = "18LX_act_pres-labeled_elev.csv"
  )
  expect_true(all(c("elev_1", "elev_2") %in% unique(tag_labeled$pressure$label)))
  tag_stap <- tag_stap(tag_labeled)
  expect_true(all(c("elev_1", "elev_2") %in% unique(tag_stap$pressure$label)))
})
