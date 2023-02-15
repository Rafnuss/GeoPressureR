library(testthat)
library(GeoPressureR)

# Load data needed for the test
static_likelihood <- readRDS(
  system.file("extdata/3_static/", "18LX_static_likelihood.rds", package = "GeoPressureR")
)

known <- data.frame(
  stap = 1,
  lat = 48.9,
  lon = 17.05
)

graph <- graph_create(
  likelihood = static_likelihood,
  known = known
)

test_that("Check graph output", {
  expect_true(all(c(
    "s", "t", "gs", "O", "sz", "lat", "lon", "stap", "flight", "flight_duration",
    "equipment", "retrieval", "mask_water", "extent"
  ) %in% names(graph)))
  expect_length(graph$s, length(graph$t))
  expect_length(graph$s, length(graph$gs))
  expect_type(graph$gs, "complex")
  expect_length(graph$sz, 3)
  expect_true(all(graph$sz == c(200, 156, 5)))
  expect_length(graph$lat, 200)
  expect_length(graph$lon, 156)
  expect_true(all(graph$step == seq_len(5)))
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
  graph_tmp <- graph_create(static_likelihood, thr_duration = 18)
  expect_true(all(graph_tmp$stap == c(1, 2, 5)))
  expect_length(graph_tmp$flight_duration, 2)
  expect_true(nrow(graph_tmp$flight[[2]]) == 3)

  expect_error(graph_create(static_likelihood, thr_duration = 10000))
})

test_that("Check create_graph() with stap", {
  graph_tmp <- graph_create(static_likelihood, stap = c(1, 2, 3, 5))
  expect_true(all(graph_tmp$stap == c(1, 2, 3, 5)))
  expect_length(graph_tmp$flight_duration, 3)
  expect_true(nrow(graph_tmp$flight[[3]]) == 2)

  expect_error(graph_create(static_likelihood, stap = c(1, 2)), NA)
  expect_error(graph_create(static_likelihood, stap = 1))
  expect_error(graph_create(static_likelihood, stap = c(1, 2, 3, 4, 5, 6)))
})


test_that("Check create_graph() for likelihood", {
  # map of prob 0 are fill up with 1
  static_likelihood_tmp <- static_likelihood
  static_likelihood_tmp[[2]]$likelihood[TRUE] <- 0
  expect_error(graph_create(static_likelihood_tmp, known = known), NA)

  # map of prob NA or NULL return an error
  static_likelihood_tmp <- static_likelihood
  static_likelihood_tmp[[2]]$likelihood[TRUE] <- NA
  expect_error(graph_create(static_likelihood_tmp), "*likelihood is invalid for*")
})


graph$T <- flight_prob(graph$gs)
test_that("Check create_graph() for likelihood", {
  graph_marginal(graph)

  # graph_add_wind

  path <- graph_simulation(graph, nj = 1)

  graph_path2lonlat(path$id, graph)

  graph_path2edge(path$id, graph)
})
