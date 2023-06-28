library(testthat)
library(GeoPressureR)

# Hide cli message
# options(cli.default_handler = function(...) { })

# Set working directory
setwd(system.file("extdata/", package = "GeoPressureR"))

test_that("workflow | full", {
  tag <- tag_create("18LX")
  tag <- tag_label(tag)

  tag <- twilight_create(tag)
  twilight_label_write(tag)
  tag <- twilight_label_read(tag)

  tag <- tag_geostap(tag,
    extent = c(-16, 23, 0, 50),
    scale = 1,
    known = data.frame(
      stap_id = 1,
      known_lon = 17.05,
      known_lat = 48.9
    )
  )

  tag <- geopressure_map(tag)
  tag <- geolight_map(tag)

  graph <- graph_create(tag) |>
    graph_add_movement()

  tag$marginal <- graph_marginal(graph)
  path <- graph_most_likely(graph)
  sim <- graph_simulation(graph)

  edge <- path2edge(path, graph)
  edge_sim <- path2edge(sim, graph)

  path <- map2path(tag)

  expect_warning(path_pres <- geopressure_timeseries(path, tag$pressure), "Requested position is on water")
})

test_that("workflow | Missing pressure value", {
  tag <- tag_create("18LX") |>
    tag_label()
  tag$pressure <- subset(tag$pressure, stap_id == 3 | stap_id == 4)

  tag <- tag_geostap(tag, extent = c(-16, 23, 0, 50), scale = 1)

  expect_warning(expect_warning(
    tag <- geopressure_map(tag),
    "*have less than 3 datapoints to be used*"
  ), "Pressure data is not on a regular interval")

  expect_equal(sapply(tag$map_pressure, is.null), c(T, T, F, F, T))

  expect_no_error(map2path(tag))
  # expect_no_error(map2path(tag, interp = 0.2))

  expect_error(graph <- graph_create(tag))
})


test_that("geopressure_mismach & likelihood | with elev_", {
  tag <- tag_create("18LX")
  tag <- tag_label(tag, file = "data/1-tag_label/18LX-labeled-elev.csv")
  tag <- tag_create(tag,
    extent = c(-16, 23, 0, 50),
    scale = 1
  )
  expect_message(tag <- geopressure_map(tag, tag$pressure), "*1|2*")
  expect_equal(length(tag$map_pressure), nrow(tag$stap))

  # Check path_pres
  path <- map2path(tag)
  path_pres <- geopressure_timeseries_latlon(
    lat = path$lat[1], lon = path$lon[1], pressure = tag$pressure[tag$pressure$stap_id == 1, ]
  )
})



test_that("workflow | modeled fewer", {
  tag <- tag_create("18LX") |> tag_label()
  expect_warning(tag <- tag_create(tag,
    extent = c(-16, 23, 0, 50),
    scale = 1,
    stap_include = c(2, 4, 5)
  ))
  tag <- geopressure_map(tag, tag$pressure)

  path <- map2path(tag)

  graph <- graph_create(tag)
  graph <- graph_add_movement(graph)


  marginal <- graph_marginal(graph)
  path <- graph_most_likely(graph)
  sim <- graph_simulation(graph)
  edge <- path2edge(path, graph)
  edge_sim <- path2edge(sim, graph)

  # too many warning
  # gts <- geopressure_timeseries(path, tag$pressure)
})
