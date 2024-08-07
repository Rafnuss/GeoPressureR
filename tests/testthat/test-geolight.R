library(testthat)
library(GeoPressureR)

# Set working directory
setwd(system.file("extdata", package = "GeoPressureR"))

tag <- tag_create("18LX", quiet = TRUE)
tag_before_label <- twilight_create(tag, twl_offset = 0)
tag <- tag_label(tag_before_label, quiet = TRUE)

test_that("Check plot_tag_twilight()", {
  expect_no_error(plot_tag_twilight(tag_before_label))
  expect_no_error(plot_tag_twilight(tag))
})

test_that("Check twilight_create()", {
  expect_true(all(c("twilight", "rise") %in% names(tag$twilight)))
  expect_true(nrow(tag$twilight) > 0)

  expect_no_error(tag_off <- twilight_create(tag, twl_offset = 2))
  expect_equal(tag_off$param$twl_offset, 2)

  expect_no_error(plot_tag_twilight(tag))
  expect_no_error(plot_tag_twilight(tag_off))

  expect_warning(tag_off <- twilight_create(tag, twl_offset = 3.5))
  plot_tag_twilight(tag_off)
})


twilight_label_write(tag, quiet = TRUE)
tag <- twilight_label_read(tag)
test_that("Check twilight_label_read() is correct", {
  expect_true("label" %in% names(tag$twilight))
})

test_that("Check geolight_map() not working before tag_set_map()", {
  expect_error(tag <- geolight_map(tag, quiet = TRUE))
})

tag <- tag_label(tag, quiet = TRUE)
tag <- tag_set_map(tag,
  extent = c(-16, 23, 0, 50),
  scale = 4,
  known = data.frame(
    stap_id = 1,
    known_lon = 17.05,
    known_lat = 48.9
  )
)

test_that("Check geolight_map() with stap", {
  tag <- geolight_map(tag, quiet = TRUE)
  expect_type(tag$map_light, "list")
  expect_equal(length(dim(tag$map_light[[1]])), 2)
})


test_that("Check non exported function", {
  sun <- GeoPressureR:::geolight_solar(tag$twilight$twilight)
  z <- GeoPressureR:::geolight_zenith(sun, lon = 0, lat = 0)
  z <- GeoPressureR:::geolight_refracted(z)
  expect_vector(z)
})
