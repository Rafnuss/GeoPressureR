library(testthat)
library(GeoPressureR)

# Load data needed for the test
static_prob_1_4 <- readRDS(
  system.file("extdata/3_static/", "18LX_static_prob_1_4.rda", package = "GeoPressureR")
)

grl <- graph_create(static_prob_1_4, thr_prob_percentile = .99, thr_gs = 150)
grl$p <- grl$ps * flight_prob(grl$gs)


test_that("Check graph", {
  # map of prob 0 are fill up with 1
  static_prob_1_4_tmp <- static_prob_1_4
  raster::values(static_prob_1_4_tmp[[2]]) <- 0
  expect_error(graph_create(static_prob_1_4_tmp), NA)

  # map of prob NA or NULL return an error
  static_prob_1_4_tmp <- static_prob_1_4
  raster::values(static_prob_1_4_tmp[[4]]) <- NA
  expect_error(graph_create(static_prob_1_4_tmp))

  grl_marginal <- graph_marginal(grl)

  # graph_add_wind

  path <- graph_simulation(grl, nj = 1)

  graph_path2lonlat(path$id, grl)

  graph_path2edge(path$id, grl)
})
