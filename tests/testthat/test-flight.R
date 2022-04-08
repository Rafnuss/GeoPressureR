test_that("check build from avonet only", {
  expect_error(flight_bird())
  expect_error(flight_bird("no_bird"))
  expect_error(flight_bird("scrocephalus"))
  expect_error(flight_bird("Acrocephalus arundinaceus"), NA)
})

test_that("check for full custum bird", {
  expect_error(flight_bird("custum_bird", mass = 1, wing_span = 1, wing_aspect = 4), NA)
  expect_error(flight_bird("custum_bird", mass = 1, wing_span = 1, wing_area = .1), NA)
  expect_error(flight_bird("custum_bird", mass = 1, wing_aspect = 4, wing_area = .1), NA)
  expect_error(
    flight_bird("custum_bird", mass = 1, wing_span = 1, wing_aspect = 4, wing_area = .1),
    NA
  ) # Note that this example is not consistant
  expect_error(flight_bird("custum_bird", mass = 1, wing_aspect = 4))
})

test_that("check hybrid call", {
  expect_error(flight_bird("Acrocephalus arundinaceus", mass = .05), NA)
  expect_error(flight_bird("Acrocephalus arundinaceus", wing_aspect = 8), NA)
  expect_error(flight_bird("Acrocephalus arundinaceus", wing_span = .25), NA)
  expect_error(flight_bird("Acrocephalus arundinaceus", body_frontal_area = .003), NA)
})

test_that("check flight_power()", {
  bird <- flight_bird("Acrocephalus arundinaceus")
  expect_error(flight_power(seq(0, 20), bird), NA)
})

test_that("check flight_prob() with groundspeed/gamma", {
  speed <- seq(0, 100)
  expect_error(flight_prob(speed), NA)
  expect_error(flight_prob(speed + 1 * 1i), NA)
  expect_error(flight_prob(speed, shape = 8, scale = 2), NA)
  expect_error(flight_prob(speed, low_speed_fix = 0), NA)
  fun_power <- function(power) {
    (1 / power)^6
  }
  expect_error(flight_prob(speed, fun_power = fun_power), NA)
})

test_that("check flight_prob() with airspeed/bird", {
  bird <- flight_bird("Acrocephalus arundinaceus")
  speed <- seq(0, 100)
  expect_error(flight_prob(speed, method = "power", bird = bird), NA)
  expect_error(flight_prob(speed + 1 * 1i, method = "power", bird = bird), NA)
  expect_error(flight_prob(speed, method = "power", bird = bird, low_speed_fix = 0), NA)
  fun_power <- function(power) {
    (1 / power)^6
  }
  expect_error(flight_prob(speed, method = "power", bird = bird, fun_power = fun_power), NA)
})
