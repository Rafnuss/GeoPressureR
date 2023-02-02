#' Progress bar function
#' @param x value of the current status
#' @param max maximum value that x will reach
#' @param text text to display
#' @noRd

progress_bar <- function(x, max = 100, text = "") {
  percent <- x / max * 100
  cat(sprintf(
    "\r[%-50s] %d / %d %s",
    paste(rep("=", percent / 2), collapse = ""),
    x, max, text
  ))
  if (x == max) {
    cat("\n")
  }
}

#' Path to data.frame
#'
#' This function convert a GeoPressureR path to a data.frame which can be read in [movevis](
#' https://movevis.org/index.html) for instance.
#'
#' The function basically duplicate location position at the start and end time of each stationary
#' period.
#'
#' @param pam pam logger dataset list with `pam$sta` computed (see `pam_sta`)
#' @param path data.frame containtings the path(s) of the bird with column `lat`, `lon` and `sta_id`
#' at least. Path can be generated with `map2path`, `graph_simulation`, `geopressureviz`
#' .
#'
#' @examples
#' pam <- pam_read(
#'   pathname = system.file("extdata/0_PAM/18LX", package = "GeoPressureR")
#' )
#' pam <- trainset_read(pam,
#'   pathname = system.file("extdata/1_pressure/labels", package = "GeoPressureR")
#' )
#' pam <- pam_sta(pam)
#' pressure_likelihood_1 <- readRDS(system.file("extdata/1_pressure/", "18LX_pressure_likelihood_1.rda",
#'   package = "GeoPressureR"
#' ))
#' path <- map2path(list(pressure_likelihood_1))
#' path2df(pam, path)
#' @seealso [`movevis::df2move`](https://movevis.org/reference/df2move.html),
#' [`map2path`], [`graph_simulation`], [`geopressureviz`]
#' @export
path2df <- function(pam, path) {

  # Check input variable
  assertthat::assert_that(is.list(pam))
  assertthat::assert_that(assertthat::has_name(pam, "sta"))
  assertthat::assert_that(is.data.frame(pam$sta))
  assertthat::assert_that(assertthat::has_name(pam$sta, c("sta_id", "start", "end")))

  assertthat::assert_that(is.list(path))
  assertthat::assert_that(assertthat::has_name(path, c("lat", "lon", "sta_id")))

  if (is.matrix(path$lat)) {
    df0 <- data.frame(
      lat = utils::stack(as.data.frame(t(path$lat)))$values,
      lon = utils::stack(as.data.frame(t(path$lon)))$values,
      sta_id = rep(path$sta_id, dim(path$lat)[1]),
      track_id = paste0(pam$id, "_", rep(seq(1, dim(path$lat)[1]), dim(path$lat)[2]))
    )
    df0 <- merge(df0, pam$sta, by = "sta_id")
  } else {
    df0 <- merge(as.data.frame(path), pam$sta, by = "sta_id")
    df0$track_id <- pam$id
  }

  df1 <- df0
  df1$time <- df1$start
  df2 <- df0
  df2$time <- df2$end

  df <- rbind(df1, df2)
  df[, names(df) %in% c("time", "track_id", "lat", "lon")]
}


#' Return the most likely path from a likelihood map
#'
#' Find the latitude and longitude of the highest value in wach map from a list of likelihood map:
#' `pressure_likelihood` or `light_likelihood`.
#'
#' `interp` can be used to interpolate unrealistic position from short stationary period based on
#' the position of the longer ones. The interpolation assumes that the first and last stationary
#' period can be safely estimated from the probability map.
#'
#' @param likelihood_map List of likelihood map of each stationary period. See
#' [`geopressure_likelihood()`] or [`geolight_likelihood()`].
#' @param interp The position of the stationary period shorter than `interp` will be
#' replace by a linear average from other position (in days) .
#' @param format One of `"lonlat"`, `"ind"`, `"arr.ind"`. return the path in lon-lat or indices
#' @return a data.frame of the position containing latitude (`lat`), longitude (`lon`) and the
#' stationary period id (`sta_id`) as column. Optionally, if indexes were requested, it will be
#' return. You will need to use `which.max(as.matrix(raster))` and not `which.max(raster)` to get
#' the correct location.
#' @examples
#' # See `geopressure_likelihood()` for generating pressure_likelihood
#' pressure_likelihood_1 <- readRDS(system.file("extdata/1_pressure/",
#'   "18LX_pressure_likelihood_1.rda",
#'   package = "GeoPressureR"
#' ))
#' map2path(list(pressure_likelihood_1))
#' @seealso [`geopressure_likelihood()`], [`geopressure_timeseries_path()`], [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#compute-altitude)
#' @export
map2path <- function(likelihood_map,
                     interp = 0,
                     format = "lonlat") {
  assertthat::assert_that(is.list(likelihood_map))
  assertthat::assert_that(is.list(likelihood_map[[1]]))
  assertthat::assert_that(inherits(likelihood_map[[1]]$map, "RasterLayer"))
  assertthat::assert_that(is.numeric(likelihood_map[[1]]$sta_id))
  assertthat::assert_that(is.numeric(interp))
  assertthat::assert_that(interp >= 0)
  assertthat::assert_that(any(format %in% c("lonlat", "ind", "arr.ind")))

  # Set the initial path to the most likely from static prob
  # There is a difference between which.max(r$map) and which.max(as.matrix(r$map)) which appeared
  # to be necessary to get correctly the position. Not really practical, maybe the way lat lon are
  # index in a raster.
  path <- do.call("rbind", lapply(likelihood_map, function(r) {
    if (format == "lonlat") {
      pos <- terra::xyFromCell(r$map, terra::which.max(r$map))
      p <- data.frame(
        lon = pos[1],
        lat = pos[2]
      )
    } else {
      pos <- arrayInd(which.max(terra::as.matrix(r$map)), dim(r$map))
      p <- data.frame(
        lon = pos[2],
        lat = pos[1]
      )
    }
    p$sta_id <- r$sta_id
    return(p)
  }))

  # Interpolation for short stationary period is only performed if interp>0
  if (interp > 0) {
    if (assertthat::has_name(likelihood_map[[1]], "temporal_extent")){
      stop("`temporal_extent` is required in likelihood_map to perform an interpolation")
    }

    # remove short stationary period
    duration <- unlist(lapply(likelihood_map, function(r) {
      as.numeric(difftime(r$temporal_extent[2],
                          r$temporal_extent[1],
                          units = "days"
      ))
    }))
    id_interp <- duration < interp
    id_interp[1] <- FALSE
    id_interp[length(id_interp)] <- FALSE

    # Find the spacing between the position
    if (is.null(likelihood_map[[1]]$flight)) {
      # Or if flight duration are not available (e.g. `prob_pressure`), assumes homogeneous spacing
      # between consecutive stationary period
      x <- path$sta_id
    } else {
      # If flight are available, sum of the all flights between stationary period
      flight_duration <- unlist(lapply(likelihood_map, function(r) {
        sum(as.numeric(difftime(r$flight$end,
                                r$flight$start,
                                units = "hours"
        )))
      }))
      # Cumulate the flight duration to get a proxy of the over distance covered
      x <- c(0, cumsum(utils::head(flight_duration, -1)))
    }
    # interpolate in between
    path$lon[id_interp] <- stats::approx(x[!id_interp], path$lon[!id_interp], x[id_interp])$y
    path$lat[id_interp] <- stats::approx(x[!id_interp], path$lat[!id_interp], x[id_interp])$y

    if (format != "lonlat") {
      path <- round(path)
    }

    # Account for water position
    #
    # sf::sf_use_s2(FALSE)
    # pts <- st_as_sf(path, coords = c("lon","lat"), crs = st_crs(4326))
    # # poly <- ne_countries(returnclass="sf")
    # poly <- ne_download(category = "physical", type="land", returnclass="sf")
    # a <- st_join(pts, poly, join = st_intersects)
  }

  if (format == "ind") {
    path$ind <- (path$lon - 1) * dim(likelihood_map[[1]]$map)[1] + path$lat
  }
  return(path)
}
