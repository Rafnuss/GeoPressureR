library(testthat)
library(GeoPressureR)


test_that("check build from avonet only", {
  expect_error(bird_create())
  expect_error(bird_create("no_bird"))
  expect_error(bird_create("scrocephalus"))
  expect_error(bird_create("Acrocephalus"))
  expect_no_error(bird_create("Acrocephalus arundinaceus"))
})

test_that("check for full custum bird", {
  expect_no_error(bird_create("custum_bird", mass = 1, wing_span = 1, wing_aspect = 4))
  expect_no_error(bird_create("custum_bird", mass = 1, wing_span = 1, wing_area = .1))
  expect_no_error(bird_create("custum_bird", mass = 1, wing_aspect = 4, wing_area = .1))
  expect_no_error(bird_create("custum_bird",
    mass = 1, wing_span = 1, wing_aspect = 4,
    wing_area = .1
  )) # Note that this example is not consistent
  expect_error(bird_create("custum_bird", mass = 1, wing_aspect = 4))
})

test_that("check hybrid call", {
  expect_no_error(bird_create("Acrocephalus arundinaceus", mass = .05))
  expect_no_error(bird_create("Acrocephalus arundinaceus", wing_aspect = 8))
  expect_no_error(bird_create("Acrocephalus arundinaceus", wing_area = .01))
  expect_no_error(bird_create("Acrocephalus arundinaceus", wing_span = .25))
  expect_no_error(bird_create("Acrocephalus arundinaceus", body_frontal_area = .003))
})

test_that("check speed2power()", {
  bird <- bird_create("Acrocephalus arundinaceus")
  expect_no_error(GeoPressureR:::speed2power(seq(0, 20), bird))
})

test_that("check graph_add_movement() with groundspeed/gamma", {
  speed <- seq(0, 100)
  graph <- structure(list(
    id = NULL, s = NULL, t = NULL, gs = 1, obs = NULL, sz = NULL, stap = NULL, equipment = NULL,
    retrieval = NULL, extent = NULL, scale = NULL, mask_water = NULL
  ), class = "graph")
  graph <- graph_add_movement(graph)
  expect_no_error(speed2prob(speed, graph$movement))
  expect_no_error(speed2prob(speed + 1 * 1i, graph$movement))
  expect_no_error(graph_add_movement(graph, shape = 8, scale = 2))
  expect_no_error(graph_add_movement(graph, low_speed_fix = 0))
  expect_no_error(graph_add_movement(graph, power2prob = \(power) (1 / power)^6))
})

test_that("check speed2prob() with groundspeed/logis", {
  speed <- seq(0, 100)
  graph <- structure(list(
    id = NULL, s = NULL, t = NULL, gs = 1, obs = NULL, sz = NULL, stap = NULL, equipment = NULL,
    retrieval = NULL, extent = NULL, scale = NULL, mask_water = NULL
  ), class = "graph")

  expect_no_error(graph_add_movement(graph, method = "logis"))
  expect_no_error(graph_add_movement(graph, shape = 8, scale = 2, method = "logis"))
  expect_no_error(graph_add_movement(graph, low_speed_fix = 0, method = "logis"))
})

test_that("check speed2prob() with airspeed/bird", {
  graph <- structure(list(
    id = NULL, s = NULL, t = NULL, gs = 1, ws = 1, obs = NULL, sz = NULL, stap = NULL,
    equipment = NULL, retrieval = NULL, extent = NULL, scale = NULL, mask_water = NULL
  ), class = "graph")
  bird <- bird_create("Acrocephalus arundinaceus")
  expect_no_error(graph_add_movement(graph, bird = bird))
  expect_no_error(graph_add_movement(graph, bird = bird, power2prob = \(power) (1 / power)^6))
  expect_no_error(graph_add_movement(graph, bird = bird, low_speed_fix = 0))
})
