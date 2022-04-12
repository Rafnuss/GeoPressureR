library(testthat)
library(GeoPressureR)

pam_data <- pam_read(
  pathname = system.file("extdata", package = "GeoPressureR"),
  crop_start = "2017-06-20", crop_end = "2018-05-02"
)

test_that("Check find_twilights()", {
  twl <- find_twilights(pam_data$light)
  expect_true(all(c("twilight", "rise") %in% names(twl)))
  expect_true(nrow(twl) > 0)
})
