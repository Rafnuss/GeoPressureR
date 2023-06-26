library(testthat)
library(GeoPressureR)

# Set working directory
setwd(system.file("extdata/", package = "GeoPressureR"))

tag <- tag_create("18LX")

twilight <- twilight_create(tag$light, twl_offset = 0)

test_that("Check twilight_create()", {
  expect_true(all(c("twilight", "rise") %in% names(twilight)))
  expect_true(nrow(twilight) > 0)
})


tag <- tag_label(tag)
geostap <- geostap_create(tag,
  extent = c(-16, 23, 0, 50),
  scale = 4,
  known = data.frame(
    stap_id = 1,
    known_lon = 17.05,
    known_lat = 48.9
  )
)

twilight <- twilight_create(tag$light, twl_offset = 0)
twilight$label <- ""


test_that("Check geolight_map() with sta", {
  geostap <- geolight_map(geostap, tag$twilight)
  expect_type(geostap$map_pressure_light, "list")
  expect_equal(length(dim(geostap$map_pressure_light[[1]])), 2)
})


test_that("Check non exported function", {
  sun <- GeoPressureR:::geolight_solar(twilight$twilight)
  z <- GeoPressureR:::geolight_zenith(sun, lon = 0, lat = 0)
  z <- GeoPressureR:::geolight_refracted(z)
  expect_vector(z)
})
