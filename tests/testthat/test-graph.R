library(testthat)
library(GeoPressureR)

# Load data needed for the test
pressure_mismatch <- readRDS(
  system.file("extdata/1_pressure/", "18LX_pressure_mismatch.rds", package = "GeoPressureR")
)
pressure_likelihood <- geopressure_likelihood(pressure_mismatch)

known <- data.frame(
  stap = 1,
  lat = 48.9,
  lon = 17.05
)

graph <- graph_create(
  likelihood = pressure_likelihood,
  known = known
)

test_that("Check graph output", {
  expect_true(all(c(
    "s", "t", "gs", "obs", "sz", "lat", "lon", "stap_model", "stap", "flight", "flight_duration",
    "equipment", "retrieval", "mask_water", "extent"
  ) %in% names(graph)))
  expect_length(graph$s, length(graph$t))
  expect_length(graph$s, length(graph$gs))
  expect_type(graph$gs, "complex")
  expect_length(graph$sz, 3)
  expect_true(all(dim(graph$obs) == graph$sz))
  expect_true(all(graph$sz == c(200, 156, 5)))
  expect_length(graph$lat, 200)
  expect_length(graph$lon, 156)
  expect_true(all(graph$stap_model == seq_len(5)))
  expect_s3_class(graph$stap, "data.frame")
  expect_true(nrow(graph$stap) == 5)
  expect_length(graph$flight, 4)
  expect_true(all(c("start", "end", "stap_s", "stap_t") %in% names(graph$flight[[1]])))
  expect_length(graph$flight_duration, 4)
  expect_type(graph$flight_duration, "double")
  expect_length(graph$equipment, 1)
  expect_true(length(graph$retrieval) > 0)
  expect_true(all(dim(graph$mask_water) == c(200, 156)))
  expect_true(all(graph$extent == c(-16, 23, 0, 50)))
})

test_that("Check create_graph() with thr_duration", {
  graph_tmp <- graph_create(pressure_likelihood, thr_duration = 18)
  expect_true(all(graph_tmp$stap_model == c(1, 2, 5)))
  expect_length(graph_tmp$flight_duration, 2)
  expect_true(nrow(graph_tmp$flight[[2]]) == 3)

  expect_error(graph_create(pressure_likelihood, thr_duration = 10000))
})

graph_short <- graph_create(pressure_likelihood, stap = c(1, 2, 3, 5))
test_that("Check create_graph() with stap_model", {
  expect_true(all(graph_short$stap_model == c(1, 2, 3, 5)))
  expect_length(graph_short$flight_duration, 3)
  expect_true(nrow(graph_short$flight[[3]]) == 2)

  expect_error(graph_create(pressure_likelihood, stap_model = c(1, 2)), NA)
  expect_error(graph_create(pressure_likelihood, stap_model = 1))
  expect_error(graph_create(pressure_likelihood, stap_model = c(1, 2, 3, 4, 5, 6)))
})


test_that("Check create_graph() for likelihood", {
  # map of prob 0 are fill up with 1
  pressure_likelihood_tmp <- pressure_likelihood
  pressure_likelihood_tmp[[2]]$likelihood[TRUE] <- 0
  expect_error(graph_create(pressure_likelihood_tmp, known = known), NA)

  # map of prob NA or NULL return an error
  pressure_likelihood_tmp <- pressure_likelihood
  pressure_likelihood_tmp[[2]]$likelihood[TRUE] <- NA
  expect_error(graph_create(pressure_likelihood_tmp), "*likelihood is invalid for*")
})


graph <- graph_add_movement(graph)

test_that("Check graph_add_movement()", {
  expect_equal(graph$movement$type, "gs")
  graph <- graph_add_movement(graph, method = "logis")
  expect_equal(graph$movement$method, "logis")
})


test_that("Check graph_marginal()", {
  marginal <- graph_marginal(graph)
  expect_length(marginal, 5)
  expect_length(marginal[[1]], 5)
  expect_true(all(c("start", "end", "stap", "marginal", "extent") %in% names(marginal[[1]])))

  graph_short <- graph_add_movement(graph_short)
  marginal <- graph_marginal(graph_short)
  expect_length(marginal, 5)
})


test_that("Check graph_simulation()", {
  simulation <- graph_simulation(graph)
  expect_true(all(c("id", "lat", "lon", "stap") %in% names(simulation)))
  expect_true(all(simulation$stap == graph$stap))

  expect_true(all(c(10, 5) == dim(simulation$lat)))
  expect_true(all(c(10, 5) == dim(simulation$lon)))
})
