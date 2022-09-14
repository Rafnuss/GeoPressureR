# remove.packages("GeoPressureR")
# devtools::install_github("rafnuss/GeoPressureR")

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

map_choices <- c()
map_val <- list()

if ("light_prob" %in% names(geopressureviz)) {
  map_choices <- c(map_choices, "Light")
  stopifnot(length(static_prob) == length(geopressureviz$light_prob))
  map_val[[length(map_val) + 1]] <- geopressureviz$light_prob
}
if ("pressure_prob_mismatch" %in% names(geopressureviz)) {
  map_choices <- c(map_choices, "Pressure mis.")
  stopifnot(length(static_prob) == length(geopressureviz$pressure_prob_mismatch))
  map_val[[length(map_val) + 1]] <- geopressureviz$pressure_prob_mismatch
}
if ("pressure_prob_thr" %in% names(geopressureviz)) {
  map_choices <- c(map_choices, "Pressure thres.")
  stopifnot(length(static_prob) == length(geopressureviz$pressure_prob_thr))
  map_val[[length(map_val) + 1]] <- geopressureviz$pressure_prob_thr
}
if ("pressure_prob" %in% names(geopressureviz)) {
  map_choices <- c(map_choices, "Pressure")
  stopifnot(length(static_prob) == length(geopressureviz$pressure_prob))
  map_val[[length(map_val) + 1]] <- geopressureviz$pressure_prob
}
map_choices <- c(map_choices, "Static")
map_val[[length(map_val) + 1]] <- static_prob
if ("static_prob_marginal" %in% names(geopressureviz)) {
  stopifnot(length(static_prob) == length(geopressureviz$static_prob_marginal))
  map_choices <- c(map_choices, "Marginal")
  map_val[[length(map_val) + 1]] <- static_prob_marginal
}


# Get stationary period information
sta <- do.call("rbind", lapply(static_prob, function(r) {
  mt <- raster::metadata(r)
  mt$start <- mt$temporal_extent[1]
  mt$end <- mt$temporal_extent[2]
  # mt$duration <- as.numeric(difftime(mt$end, mt$start, units = "days"))
  mt <- within(mt, rm(flight, temporal_extent, max_sample, margin))
  as.data.frame(mt)
}))


# Get the timeserie of pressure
if ("pam_data" %in% names(geopressureviz) & !("pam" %in% names(geopressureviz))){
  warning("pam_data has been deprecated in favor of pam. Please make the change in your code.")
  geopressureviz$pam = pam_data
}
stopifnot("pam" %in% names(geopressureviz))
pressure <- geopressureviz$pam$pressure
stopifnot(is.data.frame(pressure))
stopifnot("date" %in% names(pressure))
stopifnot(inherits(pressure$date, "POSIXt"))
stopifnot("obs" %in% names(pressure))
stopifnot(is.numeric(pressure$obs))
stopifnot("sta_id" %in% names(pressure))
if (!("isoutliar" %in% names(pressure))) {
  if ("isoutliar" %in% names(pressure)) {
    warning("pressure$isoutliar is deprecated in favor of pressure$isoutlier. Change your code",
            " to be back compatible with futur version.")
    pressure$isoutlier <- pressure$isoutliar
  } else{
    pressure$isoutlier <- FALSE
  }
}
gdl_id <- geopressureviz$pam$id


# Correct duration for pressure datapoint available
pres_outlier_sta <- aggregate(!pressure$isoutlier, by = list(sta_id = pressure$sta_id), FUN = sum)
res <- as.numeric(difftime(pressure$date[2], pressure$date[1], units = "days"))
id_match <- match(sta$sta_id, pres_outlier_sta$sta_id)
stopifnot(!is.na(id_match))
sta$duration <- pres_outlier_sta$x[id_match] * res

# Set color of each stationary period
col <- rep(RColorBrewer::brewer.pal(8, "Dark2"), times = ceiling(max(sta$sta_id) / 8))
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



# Get the pressure timeserie
if ("pressure_timeserie" %in% names(geopressureviz)) {
  pressure_timeserie <- geopressureviz$pressure_timeserie

  stopifnot(length(pressure_timeserie) == nrow(sta))
  p_ts_sta_id <- unlist(lapply(pressure_timeserie, function(x) {
    if (is.null(x)) {
      NA
    } else {
      median(x$sta_id[!(x$sta_id == 0)])
    }
  }))
  test <- p_ts_sta_id == sta$sta_id
  test[is.na(test)] <- TRUE
  stopifnot(test)
  ts0 <- pressure_timeserie
  ts0 <- lapply(ts0, function(x) {
    if (is.null(x)) {
      x <- data.frame(
        lon = NA,
        lat = NA,
        lt = 1
      )
    } else {
      x$lt <- 1
    }
    return(x)
  })
  path0 <- do.call("rbind", lapply(ts0, function(x) {
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
# grl <- graph_add_wind(grl, pam$pressure, '~/18IC_')
#
# # path0$gs = grl$gs
# # path0$as = grl$gs
# # path0$ws = grl$ws
