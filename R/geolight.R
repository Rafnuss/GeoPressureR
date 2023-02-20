#' Estimate twilight from continuous light data
#'
#' @description
#' Search for time of sunset and sunrise that correspond to first time light exceed and fall below
#' a given light threshold.
#'
#' Function inspired from [`TwGeos::findTwilights`](
#' https://rdrr.io/github/slisovski/TwGeos/man/findTwilights.html).
#'
#' @param light Data.frame with columns `date` and `value` that are the sequence of sample  times
#' (as POSIXct) and light levels recorded by the tag respectively (see [`tag_read()`]).
#' @param thr_light Light threshold that defines twilight. By default, it uses the smallest
#' value of light (i.e, first and last light of day).
#' @param shift_k Shift of the middle of the night compared to 00:00 UTC (in seconds). If not
#' provided, it uses the middle of all nights.
#' @return A data.frame with columns:
#' - `twilight` (date-time of twilight)
#' - `rise` (logical) indicating sunrise (`TRUE`) or sunset (`FALSE`).
#' @seealso [GeoPressureManual | Light Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/light-map.html#twilight-annotation)
#' @examples
#' tag <- tag_read(
#'   directory = system.file("extdata/0_tag/18LX", package = "GeoPressureR"),
#'   crop_start = "2017-06-20", crop_end = "2018-05-02"
#' )
#' twl <- geolight_twilight(tag$light)
#' head(twl)
#' @export
geolight_twilight <- function(light,
                              thr_light = NA,
                              shift_k = NA) {
  assertthat::assert_that(is.data.frame(light))
  assertthat::assert_that(assertthat::has_name(light, c("date", "value")))
  assertthat::assert_that(inherits(light$date, "POSIXt"))
  assertthat::assert_that(is.numeric(light$value))

  if (is.na(thr_light)) {
    thr_light <- min(light$value[light$value > 0])
  }
  assertthat::assert_that(is.numeric(thr_light))

  # add padding of time to center if night are not at 00:00 UTC
  if (is.na(shift_k)) {
    mat <- geolight_light2mat(light, shift_k = 0)
    res <- as.numeric(difftime(mat$date[2], mat$date[1], units = "secs"))
    l <- mat$value >= thr_light
    tmp <- rowMeans(l, na.rm = TRUE)
    shift_id <- round(sum(tmp * seq_len(dim(mat$value)[1])) / sum(tmp))
    shift_k <- res * shift_id - 60 * 60 * 12
  }

  # Use geolight_light2mat() to reshape light into a matrix
  mat <- geolight_light2mat(light, shift_k)

  # Compute exceed of light
  l <- mat$value >= thr_light
  # terra::image(l)

  # Find the first light
  id_sr <- apply(l, 2, which.max)
  if (sum(id_sr == 1) > 1) {
    cli::cli_warn(
      "There is likely a problem with the shiftK, ", sum(id_sr == 1),
      " twilights set at midnight. shift_k=", shift_k
    )
  }
  id_sr_r <- id_sr + (seq_len(dim(l)[2]) - 1) * dim(l)[1]
  sr <- as.POSIXct(mat$date[id_sr_r], origin = "1970-01-01", tz = "UTC")

  id_ss <- dim(l)[1] - apply(l[nrow(l):1, ], 2, which.max)
  if (sum(id_ss == 1) > 1) {
    cli::cli_warn(
      "There is likely a problem with the shiftK, ", sum(id_ss == 1),
      " twilights set at midnight. shift_k=", shift_k
    )
  }
  id_ss_s <- id_ss + (seq_len(dim(l)[2]) - 1) * dim(l)[1]
  ss <- as.POSIXct(mat$date[id_ss_s], origin = "1970-01-01", tz = "UTC")

  twl <- data.frame(
    twilight = c(ss, sr),
    rise = c(!logical(length(ss)), logical(length(sr)))
  )

  # order by time
  twl <- twl[order(twl$twilight), ]

  return(twl)
}



#' Compute likelihood map from twilights
#'
#' @description
#' This functions uses a data.frame of twilight to estimate a list of likelihood map for each
#' stationary periods. The functions performs the following steps
#'
#' 1. Perform a calibration on the stationary period `stap_calib`.
#' 2. Using the calibration, a likelihood map is computed for each twilight.
#' 3. Using the stationary periods dates, all likelihood map of the same calibration period are
#' combined.
#'
#' # Calibration
#'
#' Instead of calibrating the twilight errors in terms of duration, we directly model the zenith
#' angle error.
#' The `adjust_calib` parameter allows to manually set how smooth you want the fit to be.
#' Because the zenith angle error model is fitted with data only at the calibration site and that
#' we are using it for all locations of the bird’s journey, it is safer to assume a broader/smoother
#' distribution.
#'
#' To check the calibration, use `fit_z_return=TRUE` to interrupt the function after the calibration
#' and return the model fit.
#'
#' # Log-linear pooling of the twilight likelihood map
#'
#' The twilights map are aggregated by stationary period according to the date time defined in
#' `tag$stap` and twilights. See [GeoPressureManual | Probability aggregation](
#' https://raphaelnussbaumer.com/GeoPressureManual/probability-aggregation.html#probability-aggregation-1)
#' for more information on probability aggreagation using log-linear pooling.
#'
#' @param tag Data logger list  with `stap` (see [`tag_read()`] and [`tag_stap()`]).
#' @param twl A data.frame with columns `twilight` (date-time of twilights) and `discard`
#' (see [`geolight_twilight()`]).
#' @param stap_calib Stationary period of the calibration.
#' @param lon_calib Longitude of the calibration site.
#' @param lat_calib Latitude of the calibration site.
#' @param extent Geographical extent of the map on which the likelihood will be computed. Vector of
#' length 4 `c(xmin, xmax, ymin, ymax)` or `c(W, E, S, N)`.
#' @param map_dim Vector length 2 with the number of row (longitude) and column (latitude)
#' @param adjust_calib Smoothing parameter for the kernel density (see [`stats::kernel()`]).
#' @param w_llp Log-linear pooling aggregation weight.
#' @param fit_z_return Logical interrupting the function after calibration and return the zenith
#' angle fit
#' @return A list for each stationary period in order 1,2,...,n containing:
#' - `stap` stationary period. Needs to be in continuous
#' - `start` POSIXct date time of the start of the stationary period
#' - `end` POSIXct date time of the end of the stationary period and start of the flight
#' - `likelihood` matrix of the likelihood map
#' - `extent` vector length 4 of the extent of the map `c(xmin, xmax, ymin, ymax)`
#' Or a kernel fit (see [`stats::kernel()`]) if `fit_z_return` is true.
#' @examples
#' # Prepare light data
#' tag <- tag_read(
#'   directory = system.file("extdata/0_tag/18LX", package = "GeoPressureR"),
#'   crop_start = "2017-06-20", crop_end = "2018-05-02"
#' )
#' twl <- geolight_twilight(tag$light)
#' csv <- read.csv(file.path(
#'   system.file("extdata/2_light/labels/", package = "GeoPressureR"),
#'   "18LX_light-labeled.csv"
#' ))
#' twl$discard <- csv$label != ""
#'
# Compute likelihood map
# light_likelihood <- geolight_likelihood(
#   tag,
#   twl,
#   stap_calib = 1,
#   lon_calib = 17.05,
#   lat_calib = 48.9,
#   extent = c(-16, 23, 0, 50),
#   map_dim = c(200, 156)
# )
#' #
# terra::plot(
#    terra::rast(light_likelihood[[1]]$likelihood,
#    extent = light_likelihood[[1]]$extent),
#    main = "Likelihood"
#  )
#' @export
geolight_likelihood <- function(tag,
                                twl,
                                stap_calib,
                                lon_calib,
                                lat_calib,
                                extent,
                                map_dim,
                                adjust_calib = 1.4,
                                w_llp = 0.1,
                                fit_z_return = FALSE) {
  assertthat::assert_that(is.list(tag))
  assertthat::assert_that(assertthat::has_name(tag, "stap"))
  assertthat::assert_that(is.data.frame(tag$stap))
  assertthat::assert_that(is.data.frame(twl))
  assertthat::assert_that(assertthat::has_name(twl, c("twilight", "discard")))
  assertthat::assert_that(inherits(twl$twilight, "POSIXt"))
  assertthat::assert_that(is.logical(twl$discard))
  assertthat::assert_that(is.numeric(lon_calib))
  assertthat::assert_that(is.numeric(lat_calib))
  assertthat::assert_that(all(stap_calib %in% tag$stap$stap))
  assertthat::assert_that(is.vector(extent))
  assertthat::assert_that(length(extent) == 4)
  assertthat::assert_that(extent[2] > extent[1])
  assertthat::assert_that(extent[4] > extent[3])
  assertthat::assert_that(length(map_dim) == 2)
  assertthat::assert_that(all(map_dim %% 1 == 0))
  assertthat::assert_that(all(map_dim > 0))
  assertthat::assert_that(is.numeric(adjust_calib))
  assertthat::assert_that(is.numeric(w_llp))

  # Assign stap to twl
  tmp <- which(mapply(function(start, end) {
    start < twl$twilight & twl$twilight < end
  }, tag$stap$start, tag$stap$end), arr.ind = TRUE)
  twl$stap <- 0
  twl$stap[tmp[, 1]] <- tmp[, 2]

  # Remove outlier
  twl_clean <- twl[!twl$discard, ]

  # Calibrate the twilight in term of zenith angle with a kernel density.
  twl_calib <- twl[twl$stap %in% stap_calib, ]
  sun_calib <- geolight_solar(twl_calib$twilight)
  z_calib <- geolight_refracted(geolight_zenith(sun_calib, lon_calib, lat_calib))
  fit_z <- stats::density(z_calib, adjust = adjust_calib, from = 60, to = 120)

  # If request to return the fit, does not continue
  if (fit_z_return) {
    return(fit_z)
  }

  # compute the likelihood of observing the zenith angle of each twilight using the calibrated
  # error function for each grid cell.

  # Compute the sun angle
  sun <- geolight_solar(twl_clean$twilight)

  # construct the grid of latitude and longitude on cell centered
  lat <- seq(extent[4], extent[3], length.out = map_dim[1] + 1)
  lat <- utils::head(lat, -1) + diff(lat[1:2]) / 2
  lon <- seq(extent[1], extent[2], length.out = map_dim[2] + 1)
  lon <- utils::head(lon, -1) + diff(lon[1:2]) / 2
  g <- data.frame(
    lon = rep(lon, each = length(lat)),
    lat = rep(lat, times = length(lon)),
    likelihood = 1
  )

  pgz <- apply(g, 1, function(x) {
    z <- geolight_refracted(geolight_zenith(sun, x[1], x[2]))
    stats::approx(fit_z$x, fit_z$y, z, yleft = 0, yright = 0)$y
  })

  # Initialize the return list from tag$stap to make sure all stap are present
  light_likelihood <- lapply(split(tag$stap, tag$stap$stap), function(l) {
    l <- as.list(l)
    # find all twilights from this stap
    id <- twl_clean$stap == l$stap
    if (sum(id) > 1) {
      g$likelihood <- exp(colSums(w_llp * log(pgz[id, ]))) # Log-linear equation express in log
    } else if (sum(id) == 1) {
      g$likelihood <- pgz[id, ]
    } else {
      g$likelihood <- 1
    }
    l$likelihood <- matrix(g$likelihood, nrow = map_dim[1], ncol = map_dim[2])
    l$extent <- extent
    return(l)
  })

  return(light_likelihood)
}


#' Convert light data in matrix format
#'
#' @param light A dataframe with columns `date` and `value` that are the sequence of sample
#' times (as POSIXct) and light levels recorded by the tag.
#' @param shift_k Shift of the middle of the night compared to 00:00 UTC (in seconds). If not
#' provided, it uses the middle of all nights.
#' @return A dataframe with columns `date` and `value`.
#' @noRd
geolight_light2mat <- function(light, shift_k = 0) {
  assertthat::assert_that(is.data.frame(light))
  assertthat::assert_that(is.data.frame(light))
  assertthat::assert_that(assertthat::has_name(light, c("date", "value")))
  assertthat::assert_that(inherits(light$date, "POSIXt"))
  assertthat::assert_that(is.numeric(light$value))
  assertthat::assert_that(is.numeric(shift_k))

  res <- difftime(utils::tail(light$date, -1), utils::head(light$date, -1), units = "secs")
  if (length(unique(res)) != 1) {
    cli::cli_abort(
      "Temporal resolution of the light data is not constant. Use TwGeos::FindTwilight() ",
      "instead."
    )
  }
  res <- as.numeric(res[1])

  # Pad time to start and finish at 00:00
  date <- seq(
    from = as.POSIXct(format(light$date[1] - shift_k, "%Y-%m-%d"), tz = "UTC"),
    to = as.POSIXct(format(light$date[length(light$date)] - shift_k, "%Y-%m-%d"),
      tz = "UTC"
    ) + 60 * 60 * 24 - res,
    by = res
  )
  date <- date + shift_k

  # if light$date is not measuring at 00:00 exacly, we need to move date
  closest <- which.min(abs(date - light$date[1]))
  date <- date - (date[closest] - light$date[1])

  # Match the observation on the new grid
  value <- rep(NA, length(date))
  id <- date %in% light$date
  assertthat::assert_that(any(id))
  value[id] <- light$value

  # reshape in matrix format
  mat <- list(
    value = matrix(value, nrow = 24 * 60 * 60 / res),
    date = matrix(date, nrow = 24 * 60 * 60 / res)
  )
  # terra::image(mat$value)
  mat$date <- as.POSIXct(mat$date, origin = "1970-01-01", tz = "UTC")

  return(mat)
}


#' Solar time and declination
#'
#' Calculate solar time, the equation of time and the sine and cosine of the solar declination.
#' These are calculated using the same methods as \url{https://gml.noaa.gov/grad/solcalc/}.
#'
#' @param tm Vector of POSIXct times.
#' @return List containing the following vectors.
#' - `solar_time` solar time (degrees)
#' - `eqn_time` equation of time (minutes of time)
#' - `sin_solar_dec` sine of the solar declination
#' - `cos_solar_dec` cosine of the solar declination
#' @seealso [`geolight_zenith()`]
#' @examples
#' # Current solar time
#' geolight_solar(Sys.time())
geolight_solar <- function(tm) {
  rad <- pi / 180

  # Time as Julian day (R form)
  jd <- as.numeric(tm) / 86400.0 + 2440587.5

  # Time as Julian century [G]
  jc <- (jd - 2451545) / 36525

  # The geometric mean sun longitude (degrees) [I]
  l0 <- (280.46646 + jc * (36000.76983 + 0.0003032 * jc)) %% 360

  # Geometric mean anomaly for the sun (degrees) [J]
  m <- 357.52911 + jc * (35999.05029 - 0.0001537 * jc)

  # The eccentricity of earth's orbit [K]
  e <- 0.016708634 - jc * (0.000042037 + 0.0000001267 * jc)

  # Equation of centre for the sun (degrees) [L]
  eqctr <- sin(rad * m) * (1.914602 - jc * (0.004817 + 0.000014 * jc)) +
    sin(rad * 2 * m) * (0.019993 - 0.000101 * jc) + sin(rad * 3 * m) * 0.000289

  # The true longitude of the sun (degrees) [m]
  lambda0 <- l0 + eqctr

  # The apparent longitude of the sun (degrees) [P]
  omega <- 125.04 - 1934.136 * jc
  lambda <- lambda0 - 0.00569 - 0.00478 * sin(rad * omega)

  # The mean obliquity of the ecliptic (degrees) [Q]
  seconds <- 21.448 - jc * (46.815 + jc * (0.00059 - jc * (0.001813)))
  obliq0 <- 23 + (26 + (seconds / 60)) / 60

  # The corrected obliquity of the ecliptic (degrees) [R]
  omega <- 125.04 - 1934.136 * jc
  obliq <- obliq0 + 0.00256 * cos(rad * omega)

  # The equation of time (minutes of time) [U,V]
  y <- tan(rad * obliq / 2)^2
  eqn_time <- 4 / rad * (y * sin(rad * 2 * l0) -
    2 * e * sin(rad * m) +
    4 * e * y * sin(rad * m) * cos(rad * 2 * l0) -
    0.5 * y^2 * sin(rad * 4 * l0) -
    1.25 * e^2 * sin(rad * 2 * m))

  # The sun's declination (radians) [T]
  solar_dec <- asin(sin(rad * obliq) * sin(rad * lambda))
  sin_solar_dec <- sin(solar_dec)
  cos_solar_dec <- cos(solar_dec)

  # Solar time unadjusted for longitude (degrees) [AB!!]
  # Am missing a mod 360 here, but is only used within cosine.
  solar_time <- ((jd - 0.5) %% 1 * 1440 + eqn_time) / 4
  # solar_time <- ((jd-2440587.5)*1440+eqn_time)/4

  # Return solar constants
  list(
    solar_time = solar_time,
    eqn_time = eqn_time,
    sin_solar_dec = sin_solar_dec,
    cos_solar_dec = cos_solar_dec
  )
}


#' Solar zenith angle
#'
#' Calculate the solar zenith angle for given times and locations
#'
#' `geolight_zenith` uses the solar time and declination calculated by `geolight_solar` to compute
#' the solar zenith angle for given times and locations, using the same methods as
#' \url{https://gml.noaa.gov/grad/solcalc/}.  This function does not adjust for atmospheric
#' refraction see [`geolight_refracted`].
#' @param sun List of solar time and declination computed by `geolight_solar`.
#' @param lon Vector of longitudes.
#' @param lat Vector latitudes.
#' @return A vector of solar zenith angles (degrees) for the given locations and times.
#' @seealso [`geolight_solar()`]
#' @examples
#' # Approx location of Sydney Harbour Bridge
#' lon <- 151.211
#' lat <- -33.852
#' # Solar zenith angle for noon on the first of May 2000
#' # at the Sydney Harbour Bridge
#' s <- geolight_solar(as.POSIXct("2000-05-01 12:00:00", "EST"))
#' geolight_zenith(s, lon, lat)
geolight_zenith <- function(sun, lon, lat) {
  rad <- pi / 180

  # Suns hour angle (degrees) [AC!!]
  hour_angle <- sun$solar_time + lon - 180
  # hour_angle <- sun$solar_time%%360+lon-180

  # Cosine of sun's zenith [AD]
  cos_zenith <- (sin(rad * lat) * sun$sin_solar_dec +
    cos(rad * lat) * sun$cos_solar_dec * cos(rad * hour_angle))

  # Limit to [-1,1] [!!]
  cos_zenith[cos_zenith > 1] <- 1
  cos_zenith[cos_zenith < -1] <- -1

  # Ignore refraction correction
  acos(cos_zenith) / rad
}



#' Atmospheric refraction
#'
#' Adjust the solar zenith angle computed by [`geolight_zenith`] for the effect of atmospheric
#' refraction.
#'
#' @param zenith Zenith angle (degrees) to adjust.
#' @return Vector of zenith angles (degrees) adjusted for atmospheric refraction.
#' @seealso [`geolight_zenith()`]
geolight_refracted <- function(zenith) {
  rad <- pi / 180
  e <- 90 - zenith
  te <- tan((rad) * e)
  # Atmospheric Refraction [AF]
  r <- ifelse(e > 85, 0,
    ifelse(e > 5, 58.1 / te - 0.07 / te^3 + 0.000086 / te^5,
      ifelse(e > -0.575,
        1735 + e * (-518.2 + e *
          (103.4 + e * (-12.79 + e * 0.711))), -20.772 / te
      )
    )
  )
  # Corrected Zenith [90-AG]
  zenith - r / 3600
}
