library(testthat)
library(GeoPressureR)
library(raster)

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

twl$isoutlier <- F
twl$calib <- twl$sta_id == 1
lon_calib <- 17.05
lat_calib <- 48.9

map <- terra::raster(nrow = 66, ncol = 23, xmn = 0, xmx = 23, ymn = -16, ymx = 50)

test_that("Check geolight_likelihood() with sta", {
  likelihood_map <- geolight_likelihood(twl, lon_calib, lat_calib, map)
  expect_type(likelihood_map, "list")
  expect_length(likelihood_map, max(twl$sta_id))
  expect_s4_class(likelihood_map[[1]], "RasterLayer")

  likelihood_map <- geolight_likelihood(twl, lon_calib, lat_calib, map, sta_id = 1)
  expect_length(likelihood_map, 1)

  fit_z <- geolight_likelihood(twl, lon_calib, lat_calib, map, fit_z_return = T)
  expect_true(assertthat::has_name(fit_z, "bw"))
})
