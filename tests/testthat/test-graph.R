library(testthat)
library(GeoPressureR)

# Load data needed for the test
static_prob_1_4 <- readRDS(system.file("extdata/3_static/", "18LX_static_prob_1_4.rda", package = "GeoPressureR"))


test_that("Check graph", {
  grl <- graph_create(static_prob_1_4, thr_prob_percentile = .99, thr_gs = 150)
  grl$p <- grl$ps * flight_prob(grl$gs)

  grl_marginal <- graph_marginal(grl)

  # graph_add_wind

  path <- graph_simulation(grl, nj = 1)

  graph_path2lonlat(path$id, grl)

  graph_path2edge(path$id, grl)
})
