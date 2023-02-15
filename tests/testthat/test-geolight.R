library(testthat)
library(GeoPressureR)
library(terra)

tag <- tag_read(
  directory = system.file("extdata/0_tag/18LX", package = "GeoPressureR"),
  crop_start = "2017-06-20", crop_end = "2018-05-02"
)

twl <- geolight_twilight(tag$light, shift_k = 0)
test_that("Check geolight_twilight()", {
  expect_true(all(c("twilight", "rise") %in% names(twl)))
  expect_true(nrow(twl) > 0)
})


tag <- trainset_read(
  tag,
  directory = system.file("extdata/1_pressure/labels", package = "GeoPressureR")
)
tag <- tag_stap(tag)
twl <- geolight_twilight(tag$light, shift_k = 0)

twl$discard <- FALSE

calib_stap <- 1
lon_calib <- 17.05
lat_calib <- 48.9

test_that("Check geolight_likelihood() with sta", {
  likelihood_map <- geolight_likelihood(tag, twl, calib_stap, lon_calib, lat_calib,
    map_dim = c(66, 23),
    extent = c(0, 23, -16, 50)
  )
  expect_type(likelihood_map, "list")
  expect_length(likelihood_map, nrow(tag$stap))
  expect_type(likelihood_map[[1]], "list")
  expect_equal(length(dim(likelihood_map[[1]]$likelihood)), 2)

  fit_z <- geolight_likelihood(tag, twl, calib_stap, lon_calib, lat_calib,
    map_dim = c(66, 23),
    extent = c(0, 23, -16, 50),
    fit_z_return = TRUE
  )
  expect_true(assertthat::has_name(fit_z, "bw"))
})


test_that("Check non exported function", {
  sun <- GeoPressureR:::geolight_solar(twl$twilight)
  z <- GeoPressureR:::geolight_zenith(sun, lon = 0, lat = 0)
  z <- GeoPressureR:::geolight_refracted(z)
  expect_vector(z)
})
