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

gr <- graph_create(static_likelihood,
  known = known
)

test_that("Check graph output", {
  expect_true(all(c(
    "s", "t", "gs", "O", "sz", "lat", "lon", "stap", "flight", "flight_duration",
    "equipment", "retrieval", "mask_water", "extent"
  ) %in% names(gr)))
  expect_length(gr$s, length(gr$t))
  expect_length(gr$s, length(gr$gs))
  expect_type(gr$gs, "complex")
  expect_length(gr$sz, 3)
  expect_true(all(gr$sz == c(200, 156, 5)))
  expect_length(gr$lat, 200)
  expect_length(gr$lon, 156)
  expect_true(all(gr$step == seq_len(5)))
  expect_length(gr$flight, 4)
  expect_true(all(c("start", "end", "stap_s", "stap_t") %in% names(gr$flight[[1]])))
  expect_length(gr$flight_duration, 4)
  expect_type(gr$flight_duration, "double")
  expect_true(gr$equipment == 26596)
  expect_true(length(gr$retrieval) > 0)
  expect_true(all(dim(gr$mask_water) == c(200, 156)))
  expect_true(all(gr$extent == c(-16, 23, 0, 50)))
})

test_that("Check create_graph() with thr_duration", {
  gr_tmp <- graph_create(static_likelihood, thr_duration = 18)
  expect_true(all(gr_tmp$stap == c(1, 2, 5)))
  expect_length(gr_tmp$flight_duration, 2)
  expect_true(nrow(gr_tmp$flight[[2]]) == 3)

  expect_error(graph_create(static_likelihood, thr_duration = 10000))
})

test_that("Check create_graph() with stap", {
  gr_tmp <- graph_create(static_likelihood, stap = c(1, 2, 3, 5))
  expect_true(all(gr_tmp$stap == c(1, 2, 3, 5)))
  expect_length(gr_tmp$flight_duration, 3)
  expect_true(nrow(gr_tmp$flight[[3]]) == 2)

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


gr$T <- flight_prob(gr$gs)
test_that("Check create_graph() for likelihood", {
  gr_marginal <- graph_marginal(gr)

  # graph_add_wind

  path <- graph_simulation(gr, nj = 1)

  graph_path2lonlat(path$id, gr)

  graph_path2edge(path$id, gr)
})
