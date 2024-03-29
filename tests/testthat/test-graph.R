library(testthat)
library(GeoPressureR)

# Set working directory
setwd(system.file("extdata", package = "GeoPressureR"))

tag <- tag_create("18LX", quiet = TRUE) |>
  tag_label(quiet = TRUE) |>
  tag_set_map(
    extent = c(-16, 23, 0, 50),
    scale = 1,
    known = data.frame(
      stap_id = 1,
      known_lon = 17.05,
      known_lat = 48.9
    )
  ) |>
  geopressure_map(quiet = TRUE)

graph <- graph_create(tag, quiet = TRUE)

test_that("Check graph output", {
  expect_length(graph$s, length(graph$t))
  expect_length(graph$s, length(graph$gs))
  expect_type(graph$gs, "complex")
  expect_length(graph$sz, 3)
  expect_true(all(dim(graph$obs) == graph$sz))
  expect_true(is.data.frame(graph$stap))
  expect_true(nrow(graph$stap) == 5)
  expect_length(graph$equipment, 1)
  expect_true(length(graph$retrieval) > 0)
})



test_that("Check create_graph() for map_pressure", {
  # map of prob 0 are fill up with 1
  tag_tmp <- tag
  tag_tmp$map_pressure$data[[2]] <- matrix(0,
    nrow = dim(tag$map_pressure)[1],
    ncol = dim(tag$map_pressure)[2]
  )
  expect_no_error(graph_create(tag_tmp, quiet = TRUE))

  # map of prob NA or NULL return an error
  tag_tmp <- tag
  tag_tmp$map_pressure[[2]][TRUE] <- NA
  expect_error(graph_create(tag_tmp, quiet = TRUE), "*is invalid for the stationary *")
})


graph <- graph_set_movement(graph)

test_that("Check graph_set_movement()", {
  expect_equal(graph$param$movement$type, "gs")
  graph <- graph_set_movement(graph, method = "logis")
  expect_equal(graph$param$movement$method, "logis")
})


test_that("Check graph_marginal()", {
  marginal <- graph_marginal(graph, quiet = TRUE)
  expect_length(marginal, 5)

  # image(marginal[[4]])
})


test_that("Check graph_simulation()", {
  simulation <- graph_simulation(graph, quiet = TRUE)
  expect_true(is.data.frame(simulation))
  expect_true(all(simulation$stap_id %in% graph$stap$stap_id))
  expect_true(nrow(simulation) == 50)
})

test_that("Check graph_most_likely()", {
  path <- graph_most_likely(graph, quiet = TRUE)
  expect_true(is.data.frame(path))
  expect_true(all(path$stap_id %in% graph$stap$stap_id))
  expect_true(nrow(path) == 5)
})
