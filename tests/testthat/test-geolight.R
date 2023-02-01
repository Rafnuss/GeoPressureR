library(testthat)
library(GeoPressureR)

pam <- pam_read(
  pathname = system.file("extdata/0_PAM/18LX", package = "GeoPressureR"),
  crop_start = "2017-06-20", crop_end = "2018-05-02"
)

twl <- geolight_twilight(pam, shift_k = 0)
test_that("Check geolight_twilight()", {
  expect_true(all(c("twilight", "rise") %in% names(twl)))
  expect_true(nrow(twl) > 0)
  expect_true(nrow(geolight_twilight(pam)) > 0)
})

test_that("Check solar()", {
  sun <- geolight_solar(twl$twilight)
  z <- geolight_zenith(sun, lon = 0, lat = 0)
  z <- geolight_refracted(z)
  expect_vector(z)
})

pam <- trainset_read(
  pam,
  pathname = system.file("extdata/1_pressure/labels", package = "GeoPressureR")
)
pam <- pam_sta(pam)

twl <- geolight_twilight(pam, shift_k = 0)
test_that("Check geolight_twilight() with sta", {
  expect_true(all(c("sta_id") %in% names(twl)))
})
