
# Read input static_prob
load("/Users/raphael/Documents/GitHub/GeoPressureR/inst/geopressureviz/geopressureviz.RData")

stopifnot(exists(static_prob))
if (exists(light_prob)){
  stopifnot(length(static_prob)==lenght(light_prob))
}
if (exists(pressure_prob)){
  stopifnot(length(static_prob)==lenght(pressure_prob))
}

# Get stationay period information
sta <- do.call("rbind", lapply(static_prob, function(r) {
  mt <- metadata(r)
  mt$start <- mt$temporal_extent[1]
  mt$end <- mt$temporal_extent[2]
  mt$duration <- as.numeric(difftime(mt$end, mt$start, units = "days"))
  mt <- within(mt, rm(flight, temporal_extent, max_sample, margin))
  as.data.frame(mt)
}))

# Set color of each stationay period
col <- rep(brewer.pal(12, "Set3"), times = 10)
sta$col <- col[sta$sta_id]

# Get flight information
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

# Set the initial path
path0 <- do.call("rbind", lapply(static_prob, function(r) {
  idx <- which.max(r)
  pos <- xyFromCell(r, idx)
  data.frame(
    lon = pos[1],
    lat = pos[2]
  )
}))

# Get the timeserie of pressure if they exsit
if (exists("pressure_timeserie")) {
  stopifnot(length(pressure_timeseries)>=max(sta$sta_id))
  ts0 <- pressure_timeserie[sta$sta_id]
} else {
  ts0 <- list()
}
