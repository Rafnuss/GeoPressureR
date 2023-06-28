library(testthat)
library(GeoPressureR)

# Set working directory
setwd(system.file("extdata/", package = "GeoPressureR"))

tag <- tag_create("18LX")

tag <- twilight_create(tag, twl_offset = 0)

test_that("Check twilight_create()", {
  expect_true(all(c("twilight", "rise") %in% names(tag$twilight)))
  expect_true(nrow(tag$twilight) > 0)
})


tag <- tag_label(tag)
tag <- tag_geostap(tag,
  extent = c(-16, 23, 0, 50),
  scale = 4,
  known = data.frame(
    stap_id = 1,
    known_lon = 17.05,
    known_lat = 48.9
  )
)

tag <- twilight_create(tag, twl_offset = 0)
tag$twilight$label <- ""


test_that("Check geolight_map() with sta", {
  tag <- geolight_map(tag)
  expect_type(tag$map_light, "list")
  expect_equal(length(dim(tag$map_light[[1]])), 2)
})


test_that("Check non exported function", {
  sun <- GeoPressureR:::geolight_solar(tag$twilight$twilight)
  z <- GeoPressureR:::geolight_zenith(sun, lon = 0, lat = 0)
  z <- GeoPressureR:::geolight_refracted(z)
  expect_vector(z)
})
