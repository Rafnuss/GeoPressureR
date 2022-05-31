library(testthat)
library(GeoPressureR)

# Load data needed for the test
pam_data <- pam_read(
  pathname = system.file("extdata", package = "GeoPressureR"),
  crop_start = "2017-06-20", crop_end = "2018-05-02"
)
pam_data <- trainset_read(pam_data, pathname = system.file("extdata", package = "GeoPressureR"))
pam_data <- pam_sta(pam_data)
static_prob <- readRDS(system.file("extdata", "18LX_static_prob.rda", package = "GeoPressureR"))


test_that("Check graph", {
  grl <- graph_create(static_prob[c(1, 2, 3, 4)], thr_prob_percentile = .99, thr_gs = 150)
  grl$p <- grl$ps * flight_prob(grl$gs)

  grl_marginal <- graph_marginal(grl)

  # graph_add_wind

  path <- graph_simulation(grl, nj = 1)

  graph_path2lonlat(path$id, grl)

  graph_path2edge(path$id, grl)
})
