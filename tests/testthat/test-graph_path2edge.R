library(testthat)
library(GeoPressureR)

test_that("graph_path2edge()", {
  # Create a sample graph and path ID
  graph <- list(s = c(1, 7, 7, 14, 15, 16), t = c(7, 14, 15, 24, 24, 24), sz = c(2, 3, 4))

  path_id <- c(1, 7, 14, 24)
  edge <- graph_path2edge(path_id, graph)
  expect_equal(as.vector(edge), c(1, 2, 4))

  # two path
  path_id <- matrix(c(1, 1, 7, 7, 14, 15, 24, 24), nrow = 2)
  edge <- graph_path2edge(path_id, graph)
  expect_equal(edge, matrix(c(1, 1, 2, 3, 4, 5), nrow = 2))
})
