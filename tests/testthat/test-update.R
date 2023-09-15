library(testthat)
library(GeoPressureR)

# Hide cli message
options(cli.default_handler = function(...) { })

# Set working directory
setwd(system.file("extdata", package = "GeoPressureR"))

tag <- tag_create("18LX") |>
  tag_label() |>
  tag_set_map(
    extent = c(-16, 23, 0, 50),
    scale = 1,
    known = data.frame(
      stap_id = 1,
      known_lon = 17.05,
      known_lat = 48.9
    ),
    include_stap_id = c(1, 3, 4, 5)
  )

tag <- geopressure_map(tag, keep_mask = TRUE, keep_mse = TRUE)
tag_old <- tag

tag_new <- tag_update(tag, file = glue::glue("./data/tag-label/{tag$param$id}-labeled-updated.csv"))

test_that("tag_update() | default", {
  expect_equal(tag_old$stap$include, tag_new$stap$include)
  expect_equal(tag_old$stap$include[5], tag_new$stap$include[5])
  expect_equal(tag_new$map_pressure$stap, tag_new$stap)
  expect_equal(tag_new$map_pressure$data[[1]], tag_old$map_pressure$data[[1]])
  expect_equal(tag_new$map_pressure$data[[5]], tag_old$map_pressure$data[[5]])
  expect_equal(tag_new$param$sd, tag_old$param$sd)
})


pressurepath <- pressurepath_create(tag_old)
pressurepath_new <- pressurepath_update(pressurepath, tag_new)

test_that("tag_update() | default", {
  expect_equal(
    pressurepath$pressure_era5[pressurepath$stap_id == 5],
    pressurepath_new$pressure_era5[pressurepath_new$stap_id == 5]
  )
})
