library(testthat)
library(GeoPressureR)

# Set working directory
setwd(system.file("extdata", package = "GeoPressureR"))

# Start by computing all the necessary file for the tests
tag <- tag_create("18LX", quiet = TRUE) |> tag_label(quiet = TRUE)
stap <- tag$stap
path <- data.frame(
  stap_id = seq_len(5),
  ind = c(1652, 1554, 1407, 1611, 1409),
  lat = c(48.5, 46.5, 43.5, 39.5, 41.5),
  lon = c(17.5, 15.5, 12.5, 16.5, 13),
  model = c(TRUE, TRUE, TRUE, TRUE, TRUE),
  known = c(TRUE, FALSE, FALSE, FALSE, FALSE)
)

test_that("pressurepath_create() | single stap", {
  i_s <- c(3)
  path_i <- subset(path, stap_id %in% i_s)
  # pressure <- subset(tag$pressure, stap_id %in% i_s)

  pressurepath <- expect_no_error(
    pressurepath_create(tag, path_i,
      variable = c("altitude", "surface_pressure", "total_precipitation"),
      era5_dataset = "land",
      quiet = TRUE,
    )
  )
  expect_true(all(c(
    "date", "pressure_tag", "stap_id", "surface_pressure", "altitude", "surface_pressure_norm",
    "lat", "lon", "total_precipitation"
  ) %in% names(pressurepath)))
  expect_equal(nrow(pressurepath), sum(tag$pressure$stap_id %in% i_s))
})

test_that("pressurepath_create() | check flight", {
  i_s <- c(2, 3, 5)
  path_i <- subset(path, stap_id %in% i_s)
  # pressure <- subset(tag$pressure, stap_id %in% i_s)

  pressurepath <- expect_no_error(pressurepath_create(tag, path_i,
    era5_dataset = "land",
    quiet = TRUE
  ))
})
