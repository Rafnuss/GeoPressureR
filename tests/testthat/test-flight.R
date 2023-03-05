library(testthat)
library(GeoPressureR)


test_that("check build from avonet only", {
  expect_error(bird_create())
  expect_error(bird_create("no_bird"))
  expect_error(bird_create("scrocephalus"))
  expect_error(bird_create("Acrocephalus"))
  expect_error(bird_create("Acrocephalus arundinaceus"), NA)
})

test_that("check for full custum bird", {
  expect_error(bird_create("custum_bird", mass = 1, wing_span = 1, wing_aspect = 4), NA)
  expect_error(bird_create("custum_bird", mass = 1, wing_span = 1, wing_area = .1), NA)
  expect_error(bird_create("custum_bird", mass = 1, wing_aspect = 4, wing_area = .1), NA)
  expect_error(
    bird_create("custum_bird", mass = 1, wing_span = 1, wing_aspect = 4, wing_area = .1),
    NA
  ) # Note that this example is not consistent
  expect_error(bird_create("custum_bird", mass = 1, wing_aspect = 4))
})

test_that("check hybrid call", {
  expect_error(bird_create("Acrocephalus arundinaceus", mass = .05), NA)
  expect_error(bird_create("Acrocephalus arundinaceus", wing_aspect = 8), NA)
  expect_error(bird_create("Acrocephalus arundinaceus", wing_area = .01), NA)
  expect_error(bird_create("Acrocephalus arundinaceus", wing_span = .25), NA)
  expect_error(bird_create("Acrocephalus arundinaceus", body_frontal_area = .003), NA)
})

test_that("check speed2power()", {
  bird <- bird_create("Acrocephalus arundinaceus")
  expect_error(speed2power(seq(0, 20), bird), NA)
})

test_that("check speed2prob() with groundspeed/gamma", {
  speed <- seq(0, 100)
  expect_error(speed2prob(speed), NA)
  expect_error(speed2prob(speed + 1 * 1i), NA)
  expect_error(speed2prob(speed, shape = 8, scale = 2), NA)
  expect_error(speed2prob(speed, low_speed_fix = 0), NA)
  expect_error(speed2prob(speed, power2prob = \(power) (1 / power)^6), NA)
})

test_that("check speed2prob() with groundspeed/logis", {
  speed <- seq(0, 100)
  expect_error(speed2prob(speed, method = "logis"), NA)
  expect_error(speed2prob(speed + 1 * 1i, method = "logis"), NA)
  expect_error(speed2prob(speed, shape = 8, scale = 2, method = "logis"), NA)
  expect_error(speed2prob(speed, low_speed_fix = 0, method = "logis"), NA)
  expect_error(speed2prob(speed, power2prob = \(power) (1 / power)^6, method = "logis"), NA)
})

test_that("check speed2prob() with airspeed/bird", {
  bird <- bird_create("Acrocephalus arundinaceus")
  speed <- seq(0, 100)
  expect_error(speed2prob(speed, method = "power", bird = bird), NA)
  expect_error(speed2prob(speed + 1 * 1i, method = "power", bird = bird), NA)
  expect_error(speed2prob(speed, method = "power", bird = bird, low_speed_fix = 0), NA)
  expect_error(speed2prob(speed, method = "power", bird = bird, power2prob = \(power) (1 / power)^6), NA)
})
