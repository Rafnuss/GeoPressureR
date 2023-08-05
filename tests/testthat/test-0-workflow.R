library(testthat)
library(GeoPressureR)

# Hide cli message
# options(cli.default_handler = function(...) { })

# Set working directory
setwd(system.file("extdata/", package = "GeoPressureR"))

test_that("workflow | full", {
  tag <- tag_read("18LX")
  tag <- tag_label(tag)

  tag <- twilight_create(tag)
  twilight_label_write(tag)
  tag <- twilight_label_read(tag)

  tag <- tag_setmap(tag,
    extent = c(-16, 23, 0, 50),
    scale = 4,
    known = data.frame(
      stap_id = 1,
      known_lon = 17.05,
      known_lat = 48.9
    )
  )

  tag <- geopressure_map(tag)
  tag <- geolight_map(tag)

  path <- tag2path(tag)
  expect_no_error(tag2path(tag, interp = 0.7))

  graph <- graph_create(tag) |>
    graph_add_movement()

  marginal <- graph_marginal(graph)
  path <- graph_most_likely(graph)
  sim <- graph_simulation(graph)

  edge <- path2edge(path, graph)
  edge_sim <- path2edge(sim, graph)

  pressurepath <- pressurepath_create(tag, path)
})

test_that("workflow | Missing pressure value", {
  tag <- tag_read("18LX", quiet = T) |> tag_label(quiet = T)
  tag$pressure <- subset(tag$pressure, stap_id == 3 | stap_id == 4)

  tag <- tag_setmap(tag, extent = c(-16, 23, 0, 50), scale = 1)

  expect_warning(expect_warning(
    tag <- geopressure_map(tag),
    "*have less than 3 datapoints to be used*"
  ), "Pressure data is not on a regular interval")

  expect_equal(sapply(tag$map_pressure$data, is.null), c(T, T, F, F, T))

  expect_no_error(tag2path(tag))

  expect_error(graph <- graph_create(tag))

  tag$stap$include <- c(F, F, T, T, F)
  expect_no_error(graph <- graph_create(tag))
})


test_that("workflow | with elev_", {
  tag <- tag_read("18LX", quiet = T)
  tag <- tag_label(tag, file = "./data/tag-label/18LX-labeled-elev.csv", quiet = T)
  tag <- tag_setmap(tag,
    extent = c(-16, 23, 0, 50),
    scale = 1
  )
  expect_message(tag <- geopressure_map(tag), "*1|2*")
  expect_equal(length(tag$map_pressure), nrow(tag$stap))

  # Check pressuretimeseries
  path <- tag2path(tag)
  pressurets <- geopressure_timeseries(
    lat = path$lat[1], lon = path$lon[1], pressure = tag$pressure[tag$pressure$stap_id == 1, ]
  )
  expect_equal(unique(pressurets$label), c("", "elev_1", "elev_2"))
})



test_that("workflow | modeled fewer", {
  tag <- tag_read("18LX", quiet = T) |> tag_label(quiet = T)
  tag <- tag_setmap(tag,
    extent = c(-16, 23, 0, 50),
    scale = 1,
    include_stap_id = c(2, 4, 5)
  )
  tag <- geopressure_map(tag)

  path <- tag2path(tag)

  graph <- graph_create(tag)
  graph <- graph_add_movement(graph)


  marginal <- graph_marginal(graph)
  path <- graph_most_likely(graph)
  sim <- graph_simulation(graph)
  edge <- path2edge(path, graph)
  edge_sim <- path2edge(sim, graph)

  # too many warning
  # gts <- pressurepath_create(tag)
})
