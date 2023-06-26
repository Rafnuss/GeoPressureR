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

  geostap <- geostap_create(tag,
    extent = c(-16, 23, 0, 50),
    scale = 1,
    known = data.frame(
      stap_id = 1,
      known_lon = 17.05,
      known_lat = 48.9
    )
  ) |>
    geopressure_map(tag$pressure) |>
    geolight_map(tag$twilight)

  graph <- geostap |>
    graph_create() |>
    graph_add_movement()

  geostap$marginal <- graph_marginal(graph)
  path <- graph_most_likely(graph)
  sim <- graph_simulation(graph)

  edge <- path2edge(path, graph)
  edge_sim <- path2edge(sim, graph)

  path <- geostap2path(geostap)

  expect_warning(path_pres <- geopressure_timeseries(path, tag$pressure))
})

test_that("workflow | Missing pressure value", {
  tag <- tag_create("18LX")
  # tag$pressure <- tag$pressure[200:300, ]
  tag <- tag_label(tag)
  tag$pressure <- subset(tag$pressure, stap_id == 3 | stap_id == 4)

  expect_warning(
    expect_warning(
      geopressure_map_check(tag),
      "*data is not on a regular interval*"
    ),
    "*have less than 3 datapoints to be used*"
  )

  geostap <- geostap_create(tag, extent = c(-16, 23, 0, 50), scale = 1)
  expect_warning(expect_warning(
    geostap <- geopressure_map(geostap, tag$pressure),
    "*have less than 3 datapoints to be used*"
  ))

  expect_equal(sapply(geostap$map_pressure, is.null), c(T, T, F, F, T))

  expect_no_error(geostap2path(geostap))
  # expect_no_error(map2path(likelihood, interp = 0.2))

  expect_error(graph <- graph_create(geostap))
})


test_that("geopressure_mismach & likelihood | with elev_", {
  tag <- tag_create("18LX")
  tag <- tag_label(tag, file = "data/1-tag_label/18LX-labeled-elev.csv")
  geostap <- geostap_create(tag,
    extent = c(-16, 23, 0, 50),
    scale = 1
  )
  expect_message(geostap <- geopressure_map(geostap, tag$pressure), "*1|2*")
  expect_equal(length(geostap$map_pressure), nrow(geostap$stap))

  # Check path_pres
  path <- geostap2path(geostap)
  path_pres <- geopressure_timeseries_latlon(
    lat = path$lat[1], lon = path$lon[1], pressure = tag$pressure[tag$pressure$stap_id == 1, ]
  )
})



test_that("workflow | modeled fewer", {
  tag <- tag_create("18LX") |> tag_label()
  expect_warning(geostap <- geostap_create(tag,
    extent = c(-16, 23, 0, 50),
    scale = 1,
    stap_include = c(2, 4, 5)
  ))
  geostap <- geopressure_map(geostap, tag$pressure)

  path <- geostap2path(geostap)

  graph <- graph_create(geostap)
  graph <- graph_add_movement(graph)


  marginal <- graph_marginal(graph)
  path <- graph_most_likely(graph)
  sim <- graph_simulation(graph)
  edge <- path2edge(path, graph)
  edge_sim <- path2edge(sim, graph)

  # too many warning
  # gts <- geopressure_timeseries(path, tag$pressure)
})
