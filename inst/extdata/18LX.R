# This scripts combines all the codes from the vignettes of the packages. It generates all the
# data needed to run the vignette.

# Load library
library(GeoPressureR)
library(raster)
library(igraph)

# 1. PAM reading and labeling ----
# Read pam data
pam_data <- pam_read(
  pathname = system.file("extdata", package = "GeoPressureR"),
  crop_start = "2017-06-20", crop_end = "2018-05-02"
)

# Add labeling
pam_data <- trainset_read(pam_data, pathname = system.file("extdata", package = "GeoPressureR"))

# Compute stationary period
pam_data <- pam_sta(pam_data)










# 2. Pressure Map ----
# Compute pressure maps from GeoPressureAPI
pressure_maps <- geopressure_map(pam_data$pressure,
  extent = c(50, -16, 0, 23),
  scale = 2,
  max_sample = 250,
  margin = 30
)

# Convert to probability map
pressure_prob <- geopressure_prob_map(pressure_maps,
  s = 1,
  thr = 0.9
)

# Compute the most likely path
path <- geopressure_map2path(pressure_prob)

# Query the pressure timeserie at each path
pressure_timeserie <- geopressure_ts_path(path, pam_data$pressure, include_flight = c(0, 1))

saveRDS(pressure_maps, "inst/extdata/18LX_pressure_maps.rda")
saveRDS(pressure_prob, "inst/extdata/18LX_pressure_prob.rda")
saveRDS(pressure_timeserie, "inst/extdata/18LX_pressure_timeserie.rda")






# 3. Light Map  ----
# Define calibration information
lon_calib <- 17.05
lat_calib <- 48.9
tm_calib_1 <- c(pam_data$sta$start[1], pam_data$sta$end[1])

# Compute twilight and read labeling file
twl <- find_twilights(pam_data$light)
csv <- read.csv(paste0(system.file("extdata", package = "GeoPressureR"), "/18LX_light-labeled.csv"))
twl$deleted <- !csv$label == ""

# Extract twilight at calibration
twl_calib <- subset(twl, !deleted & twilight >= tm_calib_1[1] & twilight <= tm_calib_1[2])

# Compute zenith angle and fit distribution
sun <- solar(twl_calib$twilight)
z <- refracted(zenith(sun, lon_calib, lat_calib))
fit_z <- density(z, adjust = 1.4, from = 60, to = 120)

# Add stationay period information on the twilight
twilight_sta_id <- sapply(twl$twilight, function(x) {
  which(pam_data$sta$start < x & x < pam_data$sta$end)
})
twilight_sta_id[sapply(twilight_sta_id, function(x) length(x) == 0)] <- 0
twl$sta_id <- unlist(twilight_sta_id)

# Find grid information from pressure map
g <- as.data.frame(pressure_prob[[1]], xy = TRUE)
g$layer <- NA

# Compute zenith angle and corresponding probability on twilight
twl_clean <- subset(twl, !deleted)
sun <- solar(twl_clean$twilight)
pgz <- apply(g, 1, function(x) {
  z <- refracted(zenith(sun, x[1], x[2]))
  approx(fit_z$x, fit_z$y, z, yleft = 0, yright = 0)$y
})

# Define Log-linear Pooling
w <- 0.1

# Produce proabibility map from light data
light_prob <- c()
for (i_s in seq_len(nrow(pam_data$sta))) {
  id <- twl_clean$sta_id == pam_data$sta$sta_id[i_s]
  if (sum(id) > 1) {
    g$layer <- exp(colSums(w * log(pgz[id, ]))) # Log-linear equation express in log
  } else if (sum(id) == 1) {
    g$layer <- pgz[id, ]
  } else {
    g$layer <- 1
  }
  gr <- rasterFromXYZ(g)
  crs(gr) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  metadata(gr) <- list(
    sta_id = pam_data$sta$sta_id[i_s],
    nb_sample = sum(id)
  )
  light_prob[[i_s]] <- gr
}

saveRDS(light_prob, "inst/extdata/18LX_light_prob.rda")













# 4. Preparing Data  ----

# Combine light and pressure map
thr_sta_dur <- 0 # in hours
sta_pres <- unlist(lapply(pressure_prob, function(x) raster::metadata(x)$sta_id))
sta_light <- unlist(lapply(light_prob, function(x) raster::metadata(x)$sta_id))
sta_thres <- pam_data$sta$sta_id[difftime(pam_data$sta$end, pam_data$sta$start, units = "hours")
                                 > thr_sta_dur]
# Get the sta_id present on all three data sources
sta_id_keep <- intersect(intersect(sta_pres, sta_light), sta_thres)
# Filter pressure and light map
pressure_prob <- pressure_prob[sta_pres %in% sta_id_keep]
light_prob <- light_prob[sta_light %in% sta_id_keep]

# Compute flight information
flight <- list()
for (i_f in seq_len(length(sta_id_keep) - 1)) {
  from_sta_id <- sta_id_keep[i_f]
  to_sta_id <- sta_id_keep[i_f + 1]
  flight[[i_f]] <- list(
    start = pam_data$sta$end[seq(from_sta_id, to_sta_id - 1)],
    end = pam_data$sta$start[seq(from_sta_id + 1, to_sta_id)],
    sta_id = seq(from_sta_id, to_sta_id - 1)
  )
}
flight[[i_f + 1]] <- list()

# Compute static prob
static_prob <- mapply(function(light, pressure, flight) {
  # define static prob as the product of light and pressure prob
  static_prob <- light * pressure

  # define metadata
  metadata(static_prob) <- metadata(pressure)
  metadata(static_prob)$flight <- flight

  # return
  static_prob
}, light_prob, pressure_prob, flight)

# Add known position
lat <- seq(raster::ymax(static_prob[[1]]), raster::ymin(static_prob[[1]]),
           length.out = nrow(static_prob[[1]]) + 1)
lat <- lat[seq_len(length(lat) - 1)] + diff(lat[1:2]) / 2
lon <- seq(raster::xmin(static_prob[[1]]), raster::xmax(static_prob[[1]]),
           length.out = ncol(static_prob[[1]]) + 1)
lon <- lon[seq_len(length(lon) - 1)] + diff(lon[1:2]) / 2

lon_calib_id <- which.min(abs(lon_calib - lon))
lat_calib_id <- which.min(abs(lat_calib - lat))

tmp <- as.matrix(static_prob[[1]])
tmp[!is.na(tmp)] <- 0
tmp[lat_calib_id, lon_calib_id] <- 1
values(static_prob[[1]]) <- tmp
tmp <- as.matrix(static_prob[[length(static_prob)]])
tmp[!is.na(tmp)] <- 0
tmp[lat_calib_id, lon_calib_id] <- 1
values(static_prob[[length(static_prob)]]) <- tmp

# Compute the most likely path
path <- geopressure_map2path(static_prob)
static_timeserie <- geopressure_ts_path(path, pam_data$pressure)

# Downscale map
# static_prob <- lapply(static_prob, function(raster) {
#   raster_ds <- aggregate(raster, fact = 1, fun = max, na.rm = T, expand = T)
#   # keep metadata
#   metadata(raster_ds) <- metadata(raster)
#   return(raster_ds)
# })

saveRDS(static_prob, "inst/extdata/18LX_static_prob.rda")
saveRDS(static_timeserie, "inst/extdata/18LX_static_timeserie.rda")






# 4-5. Basic and wind Graph ----

# Location of wind data
dir_save <- "~"

# create graph
grl <- graph_create(static_prob, thr_prob_percentile = .99, thr_gs = 150)

# Add wind data
filename <- paste0(dir_save, "/", "18IC_")
grl <- graph_add_wind(grl, pressure = pam_data$pressure, filename, thr_as = 100)

# Compute the probability
bird <- flight_bird("Acrocephalus arundinaceus")
grl$p <- grl$ps * flight_prob(grl$as, method = "power", bird = bird, low_speed_fix = 10)

# Shortest path
g <- graph_from_data_frame(data.frame(from = grl$s, to = grl$t, weight = -log(grl$p)))
sp <- shortest_paths(g, from = paste(grl$equipement), to = paste(grl$retrival))
grl$shortest_path <- graph_path2lonlat(as.numeric(sp$vpath[[1]]$name), grl)

# Pressure timeserie at the best math
shortest_path <- as.data.frame(grl$shortest_path)
shortest_path_timeserie <- geopressure_ts_path(shortest_path, pam_data$pressure)

# Marginal
grl_marginal <- graph_marginal(grl)

saveRDS(grl, "inst/extdata/18LX_grl.rda")
saveRDS(grl_marginal, "inst/extdata/18LX_grl_marginal.rda")
saveRDS(shortest_path_timeserie, "inst/extdata/18LX_shortest_path_timeserie.rda")


# Export for GeoPressureViz
grl <- readRDS(system.file("extdata", "18LX_grl.rda", package = "GeoPressureR"))
static_prob <- readRDS(system.file("extdata", "18LX_static_prob.rda", package = "GeoPressureR"))
static_prob_marginal <- readRDS(system.file("extdata", "18LX_grl_marginal.rda",
                                            package = "GeoPressureR"))
shortest_path_timeserie <- readRDS(system.file("extdata", "18LX_shortest_path_timeserie.rda",
                                               package = "GeoPressureR"))
light_prob <- readRDS(system.file("extdata", "18LX_light_prob.rda", package = "GeoPressureR"))
pressure_prob <- readRDS(system.file("extdata", "18LX_pressure_prob.rda", package = "GeoPressureR"))

sta_marginal <- unlist(lapply(static_prob_marginal, function(x) raster::metadata(x)$sta_id))
sta_pres <- unlist(lapply(pressure_prob, function(x) raster::metadata(x)$sta_id))
sta_light <- unlist(lapply(light_prob, function(x) raster::metadata(x)$sta_id))
pressure_prob <- pressure_prob[sta_pres %in% sta_marginal]
light_prob <- light_prob[sta_light %in% sta_marginal]


geopressureviz <- list(
  pam_data = pam_data,
  static_prob = static_prob,
  static_prob_marginal = static_prob_marginal,
  pressure_prob = pressure_prob,
  light_prob = light_prob,
  pressure_timeserie = shortest_path_timeserie
)
save(geopressureviz, file = "inst/geopressureviz/geopressureviz.RData")
