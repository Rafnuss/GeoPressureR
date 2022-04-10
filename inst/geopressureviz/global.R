
suppressMessages({
  library(BiocManager)
  options(repos = BiocManager::repositories())
  library(plotly)
  library(shinyjs)
  library(GeoPressureR)
  library(shiny)
  library(leaflet)
  library(plotly)
  library(rgdal)
  library(leaflet.extras)
  library(raster)
  library(shinyWidgets)
  library(geosphere)
  library(RColorBrewer)
  library(readr)
})

# Read input
stopifnot(file.exists("~/geopressureviz.RData"))
load("~/geopressureviz.RData")

stopifnot("static_prob" %in% names(geopressureviz))
static_prob <- geopressureviz$static_prob

if ("light_prob" %in% names(geopressureviz)) {
  light_prob <- geopressureviz$light_prob
  stopifnot(length(static_prob) == length(light_prob))
} else {
  light_prob <- NA
}

if ("pressure_prob" %in% names(geopressureviz)) {
  pressure_prob <- geopressureviz$pressure_prob
  stopifnot(length(static_prob) == length(pressure_prob))
} else {
  pressure_prob <- NA
}



# Get stationay period information
sta <- do.call("rbind", lapply(static_prob, function(r) {
  mt <- raster::metadata(r)
  mt$start <- mt$temporal_extent[1]
  mt$end <- mt$temporal_extent[2]
  mt$duration <- as.numeric(difftime(mt$end, mt$start, units = "days"))
  mt <- within(mt, rm(flight, temporal_extent, max_sample, margin))
  as.data.frame(mt)
}))

# Set color of each stationay period
col <- rep(RColorBrewer::brewer.pal(12, "Set3"), times = 10)
sta$col <- col[sta$sta_id]

# Get flight information and compute flight duration directly
flight <- lapply(static_prob, function(r) {
  fl <- metadata(r)$flight
  if (length(fl) > 0) {
    fl$duration <- mapply(function(s, e) {
      as.numeric(difftime(e, s, units = "hours"))
    }, fl$start, fl$end)
  } else {
    fl$duration <- 0
  }
  fl
})

# Get the timeserie of pressure
stopifnot("pam_data" %in% names(geopressureviz))
pressure <- geopressureviz$pam_data$pressure
stopifnot(is.data.frame(pressure))
stopifnot("date" %in% names(pressure))
stopifnot(inherits(pressure$date, "POSIXt"))
stopifnot("obs" %in% names(pressure))
stopifnot(is.numeric(pressure$obs))
stopifnot("sta_id" %in% names(pressure))
if (!("isoutliar" %in% names(pressure))) {
  pressure$isoutliar <- FALSE
}

# Get the pressure timeserie
if ("pressure_timeserie" %in% names(geopressureviz)) {
  pressure_timeserie <- geopressureviz$pressure_timeserie
  stopifnot(length(pressure_timeserie) <= max(sta$sta_id))
  ts0 <- pressure_timeserie[sta$sta_id]
  ts0 <- lapply(ts0, function(x) {
    x$lt <- 1
    return(x)
  })
  path0 <- do.call("rbind", lapply(pressure_timeserie, function(x) {
    data.frame(
      lon = x$lon[1],
      lat = x$lat[1]
    )
  }))
} else {
  ts0 <- list()
}


if (!exists("path0")) {
  # Set the initial path to the most likely from static prob
  path0 <- geopressure_map2path(static_prob)
}





#
#
# # Windspeed
# grl$sz <- c(nrow(static_prob[[1]]), ncol(static_prob[[1]]), length(static_prob))
# lat <- seq(raster::ymax(static_prob[[1]]), raster::ymin(static_prob[[1]]),
#            length.out = nrow(static_prob[[1]]) + 1
# )
# grl$lat <- utils::head(lat, -1) + diff(lat[1:2]) / 2
# lon <- seq(raster::xmin(static_prob[[1]]), raster::xmax(static_prob[[1]]),
#            length.out = ncol(static_prob[[1]]) + 1
# )
# grl$lon <- utils::head(lon, -1) + diff(lon[1:2]) / 2
# grl$flight <- lapply(static_prob, function(x) {
#   raster::metadata(x)$flight
# })
# flight_duration <- unlist(lapply(static_prob, function(x) {
#   mtf <- raster::metadata(x)
#   as.numeric(sum(difftime(mtf$flight$end, mtf$flight$start, units = "hours")))
# }))
#
# lonlat2path <- function(path, grl) {
#
#   ilat <- sapply(path$lat,function(x){which.min(abs(x-grl$lat))})
#   ilon <- sapply(path$lon,function(x){which.min(abs(x-grl$lon))})
#   ista <- seq(1,grl$sz[3])
#
#   id <- ilat + (ilon-1)*grl$sz[1] + (ista-1)*grl$sz[1]*grl$sz[2]
#
#   return(id)
# }
#
# tmp <- lonlat2path(path0,grl)
# grl$s <- tmp[seq_len(length(tmp)-1)]
# grl$t <- tmp[seq(2,length(tmp))]
#
# gs_abs <- geosphere::distGeo(
#   cbind(path0$lon[seq_len(length(tmp)-1)], path0$lat[seq_len(length(tmp)-1)]),
#   cbind(path0$lon[seq(2,length(tmp))], path0$lat[seq(2,length(tmp))])
# ) / 1000 / flight_duration[seq(1,length(flight_duration)-1)]
#
# gs_bearing <- geosphere::bearingRhumb(
#   cbind(path0$lon[seq_len(length(tmp)-1)], path0$lat[seq_len(length(tmp)-1)]),
#   cbind(path0$lon[seq(2,length(tmp))], path0$lat[seq(2,length(tmp))])
# )
# gs_bearing[is.na(gs_bearing)] <- 0
#
# grl$gs <- gs_abs * cos((gs_bearing - 90) * pi / 180) +
#   1i * gs_abs * sin((gs_bearing - 90) * pi / 180)
#
# grl <- graph_add_wind(grl, pam_data$pressure, '~/18IC_')
#
# # path0$gs = grl$gs
# # path0$as = grl$gs
# # path0$ws = grl$ws
