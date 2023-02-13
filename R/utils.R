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
#' @param tag data logger dataset list with `tag$sta` computed (see `tag_sta`)
#' @param path data.frame containtings the path(s) of the bird with column `lat`, `lon` and `stap`
#' at least. Path can be generated with `map2path`, `graph_simulation`, `geopressureviz`
#' .
#'
#' @examples
#' tag <- tag_read(
#'   pathname = system.file("extdata/0_tag/18LX", package = "GeoPressureR")
#' )
#' tag <- trainset_read(tag,
#'   pathname = system.file("extdata/1_pressure/labels", package = "GeoPressureR")
#' )
#' tag <- tag_stap(tag)
#' pressure_likelihood_1 <- readRDS(system.file("extdata/1_pressure/", "
#'   18LX_pressure_likelihood_1.rda",
#'   package = "GeoPressureR"
#' ))
#' path <- map2path(list(pressure_likelihood_1))
#' path2df(tag, path)
#' @seealso [`movevis::df2move`](https://movevis.org/reference/df2move.html),
#' [`map2path`], [`graph_simulation`], [`geopressureviz`]
#' @export
path2df <- function(tag, path) {
  # Check input variable
  assertthat::assert_that(is.list(tag))
  assertthat::assert_that(assertthat::has_name(tag, "sta"))
  assertthat::assert_that(is.data.frame(tag$sta))
  assertthat::assert_that(assertthat::has_name(tag$sta, c("stap", "start", "end")))

  assertthat::assert_that(is.list(path))
  assertthat::assert_that(assertthat::has_name(path, c("lat", "lon", "stap")))

  if (is.matrix(path$lat)) {
    df0 <- data.frame(
      lat = utils::stack(as.data.frame(t(path$lat)))$values,
      lon = utils::stack(as.data.frame(t(path$lon)))$values,
      stap = rep(path$stap, dim(path$lat)[1]),
      track_id = paste0(tag$id, "_", rep(seq(1, dim(path$lat)[1]), dim(path$lat)[2]))
    )
    df0 <- merge(df0, tag$sta, by = "stap")
  } else {
    df0 <- merge(as.data.frame(path), tag$sta, by = "stap")
    df0$track_id <- tag$id
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
#' @param likelihood List of likelihood map of each stationary period. See
#' [`geopressure_likelihood()`] or [`geolight_likelihood()`].
#' @param interp The position of the stationary period shorter than `interp` will be
#' replace by a linear average from other position (in days) .
#' @param format One of `"lonlat"`, `"ind"`, `"arr.ind"`. return the path in lon-lat or indices
#' @return a data.frame of the position containing latitude (`lat`), longitude (`lon`) and the
#' stationary period id (`stap`) as column. Optionally, if indexes were requested, it will be
#' return. You will need to use `which.max(as.matrix(map))` and not `which.max(map)` to get
#' the correct location.
#' @examples
#' # See `geopressure_likelihood()` for generating pressure_likelihood
#' pressure_likelihood_1 <- readRDS(system.file("extdata/1_pressure/",
#'   "18LX_pressure_likelihood_1.rda",
#'   package = "GeoPressureR"
#' ))
#' map2path(list(pressure_likelihood_1))
#' @seealso [`geopressure_likelihood()`], [`geopressure_timeseries_path()`], [GeoPressureManual |
#' Pressure Map](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#compute-altitude)
#' @export
map2path <- function(likelihood,
                     interp = 0,
                     format = "lonlat") {
  # Check if likelihood map is a map or a list of maps.
  assertthat::assert_that(is.list(likelihood))
  assertthat::assert_that(is.list(likelihood[[1]]))
  assertthat::assert_that(is.numeric(likelihood[[1]]$stap))
  assertthat::assert_that(is.numeric(interp))
  assertthat::assert_that(interp >= 0)
  assertthat::assert_that(any(format %in% c("lonlat", "ind", "arr.ind")))

  # Set the initial path to the most likely from static prob
  path <- do.call("rbind", lapply(likelihood, function(l) {
    if (!("likelihood" %in% names(l))) {
      p <- data.frame(
        lon = NA,
        lat = NA
      )
    } else if (format == "lonlat") {
      pos <- terra::as.data.frame(terra::rast(l$likelihood, extent = l$extent), xy = TRUE)
      p <- data.frame(
        lon = pos[which.max(pos[, 3]), 1],
        lat = pos[which.max(pos[, 3]), 2]
      )
    } else {
      pos <- arrayInd(which.max(l$likelihood), dim(l$likelihood))
      p <- data.frame(
        lon = pos[2],
        lat = pos[1]
      )
    }
    p$stap <- l$stap
    return(p)
  }))

  # Interpolation for short stationary period is only performed if interp>0
  if (interp > 0) {
    # compute duration
    duration <- unlist(lapply(likelihood, function(l) {
      as.numeric(difftime(l$end, l$start, units = "days"))
    }))
    # identify stap to interpolate
    id_interp <- duration < interp
    # Enforce first and last stap constant
    if (any(id_interp[c(1, length(id_interp))])) {
      id_interp[c(1, length(id_interp))] <- FALSE
      warning(paste0(
        "First and last stap are shorter than ", interp, " days but cannot be ",
        "interpolated. They will be kept as constant."
      ))
    }

    # Compute flight duration
    flight <- utils::tail(sapply(likelihood, function(l) {
      l$end
    }), -1) -
      utils::head(sapply(likelihood, function(l) {
        l$end
      }), -1)

    # Cumulate the flight duration to get a proxy of the over distance covered
    w <- c(0, flight)

    # interpolate in between
    path$lon[id_interp] <- stats::approx(w[!id_interp], path$lon[!id_interp], w[id_interp])$y
    path$lat[id_interp] <- stats::approx(w[!id_interp], path$lat[!id_interp], w[id_interp])$y

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
    path$ind <- (path$lon - 1) * dim(likelihood[[1]]$likelihood)[1] + path$lat
  }
  return(path)
}
