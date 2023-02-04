# This scripts generate the dataset used in the examples
# Load library
library(GeoPressureR)
library(terra)
library(igraph)

# 1. tag reading and labeling ----
# Read tag data
tag <- tag_read(
  pathname = "inst/extdata/0_tag/18LX",
  crop_start = "2017-06-20", crop_end = "2018-05-02"
)

# Add labeling
tag <- trainset_read(
  tag,
  pathname = "inst/extdata/1_pressure/labels",
  )

# Compute stationary period
tag <- tag_sta(tag)










# 2. Pressure Map ----
# Compute pressure maps from GeoPressureAPI
pressure_mismatch <- geopressure_mismatch(tag$pressure,
  extent = c(50, -16, 0, 23),
  scale = 4)
saveRDS(pressure_mismatch[[1]], "inst/extdata/1_pressure/18LX_pressure_mismatch_1.rda")


# Convert to probability map
pressure_likelihood <- geopressure_likelihood(pressure_mismatch)
saveRDS(pressure_likelihood[[1]], "inst/extdata/1_pressure/18LX_pressure_likelihood_1.rda")


# Compute the most likely path
p <- list(
  simple_path = map2path(pressure_likelihood),
  interp_path = map2path(pressure_likelihood, interp = 2),
  sta_duration = unlist(lapply(pressure_likelihood, function(x) {
       as.numeric(difftime(x$temporal_extent[2],
         x$temporal_extent[1],
         units = "days"
       ))
     }))
)
saveRDS(p, "inst/extdata/1_pressure/18LX_pressure_path.rda")






# 3. Light Map  ----

# Compute twilight and read labeling file
twl <- geolight_twilight(tag$light, shift_k = 0)
csv <- read.csv(paste0(system.file("extdata/2_light/labels/", package = "GeoPressureR"),
                       "18LX_light-labeled.csv"))
twl$isoutlier <- !csv$label == ""

# Extract twilight at calibration
tm_calib_1 <- c(tag$sta$start[1], tag$sta$end[1])
twl$calib <- twl$twilight >= tm_calib_1[1] & twl$twilight <= tm_calib_1[2]

# Compute likelihood map
light_likelihood <- geolight_likelihood(twl,
                    lon_calib = 17.05,
                    lat_calib = 48.9,
                    pressure_likelihood[[1]]$likelihood)












# 4. Preparing Data  ----

# Combine light and pressure map
thr_sta_dur <- 0 # in hours
sta_pres <- unlist(lapply(pressure_likelihood, function(x) x$sta_id))
sta_light <- unlist(lapply(light_likelihood, function(x) x$sta_id))
sta_thres <- tag$sta$sta_id[difftime(tag$sta$end, tag$sta$start, units = "hours")
                                 > thr_sta_dur]
# Get the sta_id present on all three data sources
sta_id_keep <- intersect(intersect(sta_pres, sta_light), sta_thres)
# Filter pressure and light map
pressure_likelihood <- pressure_likelihood[sta_pres %in% sta_id_keep]
light_likelihood <- light_likelihood[sta_light %in% sta_id_keep]

# Compute flight information
flight <- list()
for (i_f in seq_len(length(sta_id_keep) - 1)) {
  from_sta_id <- sta_id_keep[i_f]
  to_sta_id <- sta_id_keep[i_f + 1]
  flight[[i_f]] <- list(
    start = tag$sta$end[seq(from_sta_id, to_sta_id - 1)],
    end = tag$sta$start[seq(from_sta_id + 1, to_sta_id)],
    sta_id = seq(from_sta_id, to_sta_id - 1)
  )
}
flight[[i_f + 1]] <- list()

# Compute static prob
static_likelihood <- mapply(function(light, pressure, flight) {
  list(
    sta_id = light$sta_id,
    flight = flight,
    likelihood = light$likelihood * pressure$likelihood
  )
}, light_likelihood, pressure_likelihood, flight)

# Add known position
lat <- seq(terra::ymax(static_likelihood[[1]]), terra::ymin(static_likelihood[[1]]),
           length.out = nrow(static_likelihood[[1]]) + 1)
lat <- lat[seq_len(length(lat) - 1)] + diff(lat[1:2]) / 2
lon <- seq(terra::xmin(static_likelihood[[1]]), terra::xmax(static_likelihood[[1]]),
           length.out = ncol(static_likelihood[[1]]) + 1)
lon <- lon[seq_len(length(lon) - 1)] + diff(lon[1:2]) / 2

lon_calib_id <- which.min(abs(lon_calib - lon))
lat_calib_id <- which.min(abs(lat_calib - lat))

tmp <- as.matrix(static_likelihood[[1]])
tmp[!is.na(tmp)] <- 0
tmp[lat_calib_id, lon_calib_id] <- 1
values(static_likelihood[[1]]) <- tmp
tmp <- as.matrix(static_likelihood[[length(static_likelihood)]])
tmp[!is.na(tmp)] <- 0
tmp[lat_calib_id, lon_calib_id] <- 1
values(static_likelihood[[length(static_likelihood)]]) <- tmp

# Compute the most likely path
# path <- map2path(static_likelihood)
# static_timeserie <- geopressure_timeseries_path(path, tag$pressure)



static_likelihood_1_4 <- lapply(static_likelihood[c(1, 2, 3, 4)], function(raster) {
  raster_ds <- aggregate(raster, fact = 4, fun = max, na.rm = T, expand = T)
  # keep metadata
  metadata(raster_ds) <- metadata(raster)
  return(raster_ds)
})

saveRDS(static_likelihood_1_4, "inst/extdata/3_static/18LX_static_likelihood_1_4.rda")

return()


# 4-5. Basic and wind Graph ----

# create graph
grl <- graph_create(static_likelihood, thr_likelihood_percentile = .99, thr_gs = 150)

# Location of wind data


# Add wind data
filename <- "/Users/raphael/Documents/GitHub/GeoPressureManual/data/5_wind_graph/18LX/18LX_"
grl <- graph_add_wind(grl, pressure = tag$pressure, filename, thr_as = 100)

# Compute the probability
bird <- flight_bird("Acrocephalus arundinaceus")
grl$p <- grl$ps * flight_likelihood(grl$as, method = "power", bird = bird, low_speed_fix = 10)


# Shortest path
g <- graph_from_data_frame(data.frame(from = grl$s, to = grl$t, weight = -log(grl$p)))
sp <- shortest_paths(g, from = paste(grl$equipment), to = paste(grl$retrieval))
grl$shortest_path <- graph_path2lonlat(as.numeric(sp$vpath[[1]]$name), grl)

# Pressure timeserie at the best math
shortest_path <- as.data.frame(grl$shortest_path)
shortest_path_timeserie <- geopressure_timeseries_path(shortest_path, tag$pressure)

# saveRDS(shortest_path_timeserie, "inst/extdata/5_wind_graph/18LX_shortest_path_timeserie.rda")


# Marginal
static_likelihood_marginal <- graph_marginal(grl)

#saveRDS(grl, "inst/extdata/18LX_grl.rda")
#saveRDS(grl_marginal, "inst/extdata/18LX_grl_marginal.rda")
#saveRDS(shortest_path_timeserie, "inst/extdata/18LX_shortest_path_timeserie.rda")


# Export for GeoPressureViz
# grl <- readRDS(system.file("extdata", "18LX_grl.rda", package = "GeoPressureR"))
# static_likelihood <- readRDS(system.file("extdata", "18LX_static_likelihood.rda", package = "GeoPressureR"))
# static_likelihood_marginal <- readRDS(system.file("extdata", "18LX_grl_marginal.rda",
#                                             package = "GeoPressureR"))
# shortest_path_timeserie <- readRDS(system.file("extdata", "18LX_shortest_path_timeserie.rda",
#                                                package = "GeoPressureR"))
# light_likelihood <- readRDS(system.file("extdata", "18LX_light_likelihood.rda", package = "GeoPressureR"))
# pressure_likelihood <- readRDS(system.file("extdata", "18LX_pressure_likelihood.rda", package = "GeoPressureR"))

sta_marginal <- unlist(lapply(static_likelihood_marginal, function(x) terra::metadata(x)$sta_id))
sta_pres <- unlist(lapply(pressure_likelihood, function(x) terra::metadata(x)$sta_id))
sta_light <- unlist(lapply(light_likelihood, function(x) terra::metadata(x)$sta_id))
pressure_likelihood <- pressure_likelihood[sta_pres %in% sta_marginal]
light_likelihood <- light_likelihood[sta_light %in% sta_marginal]



# Query the pressure timeserie at each path
# pressure_timeserie <- geopressure_timeseries_path(path, tag$pressure, include_flight = c(0, 1))
#
# saveRDS(pressure_maps, "inst/extdata/18LX_pressure_maps.rda")
# saveRDS(pressure_likelihood, "inst/extdata/18LX_pressure_likelihood.rda")
# saveRDS(pressure_timeserie, "inst/extdata/18LX_pressure_timeserie.rda")



geopressureviz <- list(
  tag = tag,
  static_likelihood = static_likelihood,
  static_likelihood_marginal = static_likelihood_marginal,
  pressure_likelihood = pressure_likelihood,
  light_likelihood = light_likelihood,
  pressure_timeserie = shortest_path_timeserie
)
save(geopressureviz, file = "inst/geopressureviz/geopressureviz.RData")
save(geopressureviz, file = "~/geopressureviz.RData")

shiny::runApp(system.file("geopressureviz", package = "GeoPressureR"), launch.browser = getOption("browser"))
