library(testthat)
library(GeoPressureR)

skip("skip graph_add_wind")

# Set working directory
setwd(system.file("extdata", package = "GeoPressureR"))

tag <- geopressuretemplate_tag("18LX", quiet = TRUE)

test_that("tag_download_wind() | special setting", {
  tmp <- tag_download_wind(
    tag,
    variable = c("u_component_of_wind", "v_component_of_wind", "temperature"),
    include_stap_id = 1
  )

  GeoPressureR:::edge_add_wind_check(tag)
})


graph <- graph_create(tag, quiet = TRUE)

graph_5 <- graph_add_wind(
  graph,
  pressure = tag$pressure,
  rounding_interval = 5
)

graph_30 <- graph_add_wind(
  graph,
  pressure = tag$pressure,
  rounding_interval = 30
)

graph_60 <- graph_add_wind(
  graph,
  pressure = tag$pressure,
  rounding_interval = 60
)

graph_1 <- graph_add_wind(
  graph,
  pressure = tag$pressure,
  rounding_interval = 1
)

plot(abs(graph_1$ws), abs(graph_5$ws - graph_1$ws))
plot(abs(graph_1$ws), abs(graph_30$ws - graph_1$ws))
plot(abs(graph_1$ws), abs(graph_60$ws - graph_1$ws))

sd(abs(graph_30$ws - graph_1$ws))
hist(abs(graph_30$ws - graph_1$ws))


graph2 <- graph_add_wind(
  graph,
  pressure = tag$pressure,
  rounding_interval = 60,
  interp_spatial_linear = FALSE
)

graph3 <- graph_add_wind(
  graph,
  pressure = tag$pressure,
  rounding_interval = 60,
  interp_spatial_linear = TRUE
)

plot(abs(graph3$ws), abs(graph2$ws))


###

graph <- graph_add_wind(graph, pressure = tag$pressure)
graph <- graph_set_movement(
  graph,
  bird = bird_create("Acrocephalus arundinaceus")
)
path_most_likely <- graph_most_likely(graph)
path_simulation <- graph_simulation(graph)


edge_most_likely <- path2edge(path_most_likely, graph)
edge_simulation <- path2edge(path_simulation, graph)

edge_add_wind(
  graph,
  edge_s = edge_most_likely$s,
  edge_t = edge_most_likely$t,
  pressure = tag$pressure,
  return_averaged_variable = FALSE
)
