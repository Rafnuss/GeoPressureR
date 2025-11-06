library(testthat)
library(GeoPressureR)

# Set working directory
setwd(system.file("extdata", package = "GeoPressureR"))

tag <- tag_create(
  id = "18LX",
  crop_start = "2017-06-20",
  crop_end = "2018-05-02",
  quiet = TRUE
)

tag_classified <- tag_label_auto(tag)
test_that("tag_label_auto() | default", {
  expect_error(tag_label_auto("not a tag"))
  expect_true(c("label") %in% names(tag_classified$acceleration))
  expect_type(tag_classified$acceleration$label, "character")
})


test_that("tag_label_write() | default", {
  expect_error(tag_label_write("not a tag", quiet = TRUE))

  # Work under normal condition
  file_labelled <- tag_label_write(
    tag_classified,
    file = "./data/tag-label/18LX.csv",
    quiet = TRUE
  )
  csv <- utils::read.csv(file_labelled)
  expect_true(all(c("series", "timestamp", "value", "label") %in% names(csv)))
  expect_true(all(c("pressure", "acceleration") %in% csv$series))
  expect_true("flight" %in% csv$label)

  # Work even if not auto-classified
  expect_no_error(tag_label_write(tag, tempfile(), quiet = TRUE))

  # create new folder
  # expect_no_error(tag_label_write(tag, file.path(tempdir(), "/test/test.csv")))

  # Test without pressure
  tag_tmp <- tag
  tag_tmp$acceleration <- NULL
  expect_no_error(tag_label_write(tag_tmp, tempfile(), quiet = TRUE))

  # Test with ref
  tag_tmp <- tag
  tag_tmp$pressure$value_ref <- tag_tmp$pressure$value + 10
  expect_no_error(tag_label_write(tag_tmp, tempfile(), quiet = TRUE))
})


tag_labelled <- tag_label_read(tag)
test_that("tag_label_read() | default", {
  # Returned value is correct
  expect_type(tag_labelled, "list")
  expect_true(c("label") %in% names(tag_labelled$pressure))
  expect_type(tag_labelled$pressure$label, "character")
  expect_true(c("label") %in% names(tag_labelled$acceleration))
  expect_type(tag_labelled$acceleration$label, "character")

  # Return error for incorrect input
  expect_error(tag_label_read(tag, file = "not a path"))
  expect_error(tag_label_read("not a tag"))

  # Test with different labelled file size
  expect_warning(
    tag_label_read(tag, file = "./data/tag-label/18LX-labeled-diffsize.csv"),
    "*The labelization file of pressure is missing*"
  )
})

tag_labelled <- tag_label_stap(tag_labelled, quiet = TRUE)
test_that("tag_label_stap() | default", {
  # Returned value is correct
  expect_type(tag_labelled, "list")
  expect_true(c("stap") %in% names(tag_labelled))
  expect_type(tag_labelled$pressure$label, "character")
  expect_type(tag_labelled$acceleration$label, "character")
  expect_gt(nrow(tag_labelled$stap), 0)
})


test_that("tag_label_stap() | for elev", {
  tag_elev <- tag_label_read(
    tag,
    file = "./data/tag-label/18LX-labeled-elev.csv"
  )
  expect_true(all(c("elev_1", "elev_2") %in% unique(tag_elev$pressure$label)))
  tag_label_stap <- tag_label_stap(tag_elev, quiet = TRUE)
  expect_true(all(c("elev_1", "elev_2") %in% unique(tag_elev$pressure$label)))
})


test_that("tag_label_read() | no acceleration", {
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


tag_labelled <- tag_label_stap(tag_labelled, quiet = TRUE)
test_that("tag_label_stap() | default", {
  # Returned value is correct
  expect_type(tag_labelled, "list")
  expect_true(c("stap") %in% names(tag_labelled))
  expect_type(tag_labelled$pressure$label, "character")
  expect_type(tag_labelled$acceleration$label, "character")
  expect_gt(nrow(tag_labelled$stap), 0)
})


test_that("tag_label_stap() | for elev", {
  tag_elev <- tag_label_read(
    tag,
    file = "./data/tag-label/18LX-labeled-elev.csv"
  )
  expect_true(all(c("elev_1", "elev_2") %in% unique(tag_elev$pressure$label)))
  tag_label_stap <- tag_label_stap(tag_elev, quiet = TRUE)
  expect_true(all(c("elev_1", "elev_2") %in% unique(tag_elev$pressure$label)))
})


test_that("tag_label_stap() | no acceleration", {
  expect_warning(
    tag_labelled <- tag_label_read(
      tag,
      file = "./data/tag-label/18LX-labeled-no_acc.csv"
    ),
    "The labelization file does not contains label for acceleration."
  )
  expect_no_error(tag_label_stap(tag_labelled, quiet = TRUE))
})

test_that("tag_label_stap() | pressure longer than acc", {
  # Make a copy of a labeled
  tag_labelled <- tag_label_read(
    tag,
    file = "./data/tag-label/18LX-labeled.csv"
  )
  tag2 <- tag_labelled

  expect_equal(nrow(tag_label_stap(tag2, quiet = TRUE)$stap), 5)

  # Remove some acceleration data
  tag2$acceleration <- tag2$acceleration[seq(1, 3000), ]

  expect_equal(nrow(tag_label_stap(tag2, quiet = TRUE)$stap), 3)

  # Add some flight label to pressure
  tag2$pressure$label[600:650] <- "flight"

  expect_equal(nrow(tag_label_stap(tag2, quiet = TRUE)$stap), 4)

  tag2$pressure$label[50:60] <- "flight"
  expect_warning(tag_label_stap(tag2, quiet = TRUE))
})


test_that("tag_label() | default", {
  expect_no_error(tag_labelled <- tag_label(tag, quiet = TRUE))
})
