library(testthat)
library(GeoPressureR)

directory <- system.file("extdata/0_tag/18LX", package = "GeoPressureR")

tag <- tag_read(
  directory = directory,
  crop_start = "2017-06-20", crop_end = "2018-05-02"
)

test_that("Check tag_read()", {
  # Check for error with incorrect input
  expect_error(tag_read("not a path"))
  expect_type(tag_read(directory = directory), "list")
  expect_error(tag_read("not a path"))

  # Check that the return tag is correct
  expect_type(tag, "list")
  expect_true(all(c("pressure", "light", "acceleration") %in% names(tag)))
  expect_gt(nrow(tag$pressure), 0)
  expect_gt(nrow(tag$light), 0)
  expect_gt(nrow(tag$acceleration), 0)

  expect_warning(tag_read(
    directory = directory,
    pressure_filename = "wrong_file"
  ))

  # Check crop
  expect_true(nrow(tag_read(
    directory = directory,
    crop_start = "2019-06-20", crop_end = "2018-05-02"
  )$light) == 0)
})

test_that("Check tag_read() for Migrate Technology", {
  directory <- system.file("extdata/0_tag/CB621", package = "GeoPressureR")

  tag <- tag_read(
    directory = directory,
    pressure_filename = ".deg",
    light_filename = ".lux",
    acceleration_filename = NA
    # crop_start = "2017-06-20", crop_end = "2018-05-02"
  )
  expect_gt(nrow(tag$pressure), 0)
  expect_gt(nrow(tag$light), 0)
})

tag_classified <- tag_classify(tag)
test_that("Check tag_classify()", {
  expect_error(tag_classify("not a tag"))
  expect_true(c("label") %in% names(tag_classified$acceleration))
  expect_type(tag_classified$acceleration$label, "character")
})


test_that("Check trainset_write()", {
  temp_dir <- tempdir() # "~" in case to test the file

  expect_error(trainset_write("not a tag", directory = temp_dir), "*tag is not a list*")
  # Work under normal condition
  full_path <- expect_error(trainset_write(tag_classified, directory = temp_dir), NA)
  csv <- read.csv(full_path)
  expect_true(all(c("series", "timestamp", "value", "label") %in% names(csv)))
  expect_true(all(c("pressure", "acceleration") %in% csv$series))
  expect_true("flight" %in% csv$label)

  # Work even if not auto-classified
  expect_error(trainset_write(tag, directory = temp_dir), NA)

  # create new folder
  expect_error(trainset_write(tag, directory = paste0(temp_dir, "/", Sys.Date())), NA)

  # Test without pressure
  tag_tmp <- tag
  tag_tmp$acceleration <- NULL
  expect_error(trainset_write(tag_tmp, directory = temp_dir), NA)

  # Test with ref
  tag_tmp <- tag
  tag_tmp$pressure$value_ref <- tag_tmp$pressure$value + 10
  expect_error(trainset_write(tag_tmp, directory = temp_dir), NA)
})

directory <- system.file("extdata/1_pressure/labels", package = "GeoPressureR")

tag_labeled <- trainset_read(tag, directory = directory)
test_that("Check trainset_read()", {
  # Returned value is correct
  expect_type(tag_labeled, "list")
  expect_true(c("label") %in% names(tag_labeled$pressure))
  expect_type(tag_labeled$pressure$label, "character")
  expect_true(c("label") %in% names(tag_labeled$acceleration))
  expect_type(tag_labeled$acceleration$label, "character")

  # Return error for incorrect input
  expect_error(trainset_read(tag, directory = "not a path"))
  expect_error(trainset_read("not a tag", directory = directory))

  # Test with different labeled file size
  expect_warning(trainset_read(tag,
    directory = directory,
    filename = "18LX_act_pres-labeled-diffsize.csv"
  ))
})

# Warning if both acceleration and pressure have flight
expect_warning(tag_stap(tag_labeled), "*will be estimated from acceleration data and the label*")
tag_labeled$pressure$label[tag_labeled$pressure$label == "flight"] <- ""
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
  tag_elev <- trainset_read(tag,
    directory = system.file("extdata/1_pressure/labels/", package = "GeoPressureR"),
    filename = "18LX_act_pres-labeled-elev.csv"
  )
  expect_true(all(c("elev_1", "elev_2") %in% unique(tag_elev$pressure$label)))
  tag_stap <- tag_stap(tag_elev)
  expect_true(all(c("elev_1", "elev_2") %in% unique(tag_elev$pressure$label)))
})
