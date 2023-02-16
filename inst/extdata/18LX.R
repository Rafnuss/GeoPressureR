# This scripts generate the dataset used in the examples
# Load library
library(GeoPressureR)
library(terra)
library(igraph)

# 1. Tag reading and labeling ----
# Read tag data
tag <- tag_read(
  directory = "inst/extdata/0_tag/18LX",
  crop_start = "2017-06-20", crop_end = "2018-05-02"
)

# Add labeling
tag <- trainset_read(
  tag,
  directory = "inst/extdata/1_pressure/labels",
)

tag <- tag_stap(tag)







# 2. Pressure Map ----
# Compute pressure maps from GeoPressureAPI
pressure_mismatch <- geopressure_mismatch(tag,
  extent = c(-16, 23, 0, 50),
  scale = 4
)
saveRDS(pressure_mismatch, "inst/extdata/1_pressure/18LX_pressure_mismatch.rds")


# Convert to probability map
pressure_likelihood <- geopressure_likelihood(pressure_mismatch)

# Compute the path
path <- map2path(pressure_likelihood, interp = 1)

# Compute the pressure at this location.
pressure_timeseries <- geopressure_timeseries_path(
  path = path,
  pressure = tag$pressure,
  include_flight = TRUE
)

saveRDS(pressure_timeseries, "inst/extdata/1_pressure/18LX_pressure_timeseries.rds")


# 3. Light Map  ----

# Compute twilight and read labeling file
shift_k <- 0
twl <- geolight_twilight(tag$light, shift_k = shift_k)

write.csv(
  data.frame(
    series = ifelse(twl$rise, "Rise", "Set"),
    timestamp = strftime(twl$twilight, "%Y-%m-%dT00:00:00Z", tz = "UTC"),
    value = (as.numeric(format(twl$twilight, "%H")) * 60 +
      as.numeric(format(twl$twilight, "%M")) - shift_k / 60 + 60 * 12) %% (60 * 24),
    label = ifelse(is.null(twl$delete), "", ifelse(twl$delete, "Delete", ""))
  ),
  file = "inst/extdata/2_light/labels/18LX_light.csv",
  row.names = FALSE
)

csv <- read.csv(file.path(
  system.file("extdata/2_light/labels/", package = "GeoPressureR"),
  "18LX_light-labeled.csv"
))
twl$discard <- csv$label != ""

# Compute likelihood map
light_likelihood <- geolight_likelihood(tag,
  twl,
  1,
  lon_calib = 17.05,
  lat_calib = 48.9,
  extent = pressure_likelihood[[1]]$extent,
  map_dim = dim(pressure_likelihood[[1]]$likelihood)
)

# Combine Pressure and light
static_likelihood <- mapply(function(light, pressure) {
  stopifnot(light$stap == pressure$stap)
  stopifnot(all(light$extent == pressure$extent))
  pressure$likelihood <- light$likelihood * pressure$likelihood
  return(pressure)
}, light_likelihood, pressure_likelihood, SIMPLIFY = FALSE)






# Compute the most likely path
p <- list(
  simple_path = map2path(pressure_likelihood),
  interp_path = map2path(pressure_likelihood, interp = 2),
)
saveRDS(p, "inst/extdata/1_pressure/18LX_pressure_path.rds")



# 4-5. Basic and wind Graph ----

# create graph
graph <- graph_create(static_likelihood,
  thr_likelihood = .99,
  thr_gs = 150,
  known = data.frame(
    stap = 1,
    lat = 48.9,
    lon = 17.05
  )
)

# Location of wind data

# Add wind data
graph <- graph_add_wind(graph,
  pressure = tag$pressure,
  directory = "/Users/raphael/Documents/GitHub/GeoPressureManual/data/5_wind_graph/18LX/18LX_",
  thr_as = 100
)

# Compute the probability
bird <- flight_bird("Acrocephalus arundinaceus")
graph$p <- graph$ps * flight_likelihood(graph$as, method = "power", bird = bird, low_speed_fix = 10)


# Shortest path
g <- graph_from_data_frame(data.frame(from = graph$s, to = graph$t, weight = -log(graph$p)))
sp <- shortest_paths(g, from = paste(graph$equipment), to = paste(graph$retrieval))
graph$shortest_path <- graph_path2lonlat(as.numeric(sp$vpath[[1]]$name), graph)

# Pressure timeserie at the best math
shortest_path <- as.data.frame(graph$shortest_path)
shortest_path_timeserie <- geopressure_timeseries_path(shortest_path, tag$pressure)

# saveRDS(shortest_path_timeserie, "inst/extdata/5_wind_graph/18LX_shortest_path_timeserie.rda")


# Marginal
static_likelihood_marginal <- graph_marginal(graph)

# saveRDS(graph, "inst/extdata/18LX_graph.rda")
# saveRDS(graph_marginal, "inst/extdata/18LX_graph_marginal.rda")
# saveRDS(shortest_path_timeserie, "inst/extdata/18LX_shortest_path_timeserie.rda")


# Query the pressure timeserie at each path
# pressure_timeserie <- geopressure_timeseries_path(path, tag$pressure, include_flight = c(0, 1))
#
# saveRDS(pressure_maps, "inst/extdata/18LX_pressure_maps.rda")
# saveRDS(pressure_likelihood, "inst/extdata/18LX_pressure_likelihood.rda")
# saveRDS(pressure_timeserie, "inst/extdata/18LX_pressure_timeserie.rda")
