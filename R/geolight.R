
# Solar Zenith/Sunrise/Sunset calculations
#
# The functions presented here are based on code and the excel
# spreadsheet from the NOAA site
#
#       http://www.esrl.noaa.gov/gmd/grad/solcalc/
#


#' Calculate solar time, the equation of time and solar declination
#'
#' The solar time, the equation of time and the sine and cosine of
#' the solar declination are calculated for the times specified by
#' \code{tm} using the same methods as
#' \url{www.esrl.noaa.gov/gmd/grad/solcalc/}.
#' @title Solar Time and Declination
#' @param tm a vector of POSIXct times.
#' @return A list containing the following vectors.
#' \item{\code{solar_time}}{the solar time (degrees)}
#' \item{\code{eqn_time}}{the equation of time (minutes of time)}
#' \item{\code{sin_solar_dec}}{sine of the solar declination}
#' \item{\code{cos_solar_dec}}{cosine of the solar declination}
#' @seealso \code{\link{zenith}}
#' @examples
#' # Current solar time
#' solar(Sys.time())
#' @export
solar <- function(tm) {
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


#' Calculate the solar zenith angle for given times and locations
#'
#' \code{zenith} uses the solar time and declination calculated by \code{solar} to compute the solar
#' zenith angle for given times and locations, using the same methods as
#' \url{www.esrl.noaa.gov/gmd/grad/solcalc/}.  This function does not adjust for atmospheric
#' refraction see \code{\link{refracted}}.
#' @title Solar Zenith Angle
#' @param sun list of solar time and declination computed by \code{solar}.
#' @param lon vector of longitudes.
#' @param lat vector latitudes.
#' @return A vector of solar zenith angles (degrees) for the given locations and times.
#' @seealso \code{\link{solar}}
#' @examples
#' # Approx location of Sydney Harbour Bridge
#' lon <- 151.211
#' lat <- -33.852
#' # Solar zenith angle for noon on the first of May 2000
#' # at the Sydney Harbour Bridge
#' s <- solar(as.POSIXct("2000-05-01 12:00:00", "EST"))
#' zenith(s, lon, lat)
#' @export
zenith <- function(sun, lon, lat) {
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



#' Adjust the solar zenith angle for atmospheric refraction.
#'
#' Given a vector of solar zeniths computed by \code{\link{zenith}}, \code{refracted} calculates the
#'  solar zeniths adjusted for the effect of atmospheric refraction.
#'
#' \code{unrefracted} is the inverse of \code{refracted}. Given a (single) solar zenith adjusted
#' for the effect of atmospheric refraction, \code{unrefracted} calculates the solar zenith as
#' computed by \code{\link{zenith}}.
#'
#' @title Atmospheric Refraction
#' @param zenith zenith angle (degrees) to adjust.
#' @return vector of zenith angles (degrees) adjusted for atmospheric refraction.
#' @export
refracted <- function(zenith) {
  rad <- pi / 180
  elev <- 90 - zenith
  te <- tan((rad) * elev)
  # Atmospheric Refraction [AF]
  r <- ifelse(elev > 85, 0,
    ifelse(elev > 5, 58.1 / te - 0.07 / te^3 + 0.000086 / te^5,
      ifelse(elev > -0.575,
        1735 + elev * (-518.2 + elev *
          (103.4 + elev * (-12.79 + elev * 0.711))), -20.772 / te
      )
    )
  )
  # Corrected Zenith [90-AG]
  zenith - r / 3600
}


#' Returns twilights for each day based on a threshold of light
#'
#' Search for pairs of sunset, sunrise that correspond to a given light threshold. Function inspired
#' from [`findTwilights`](https://rdrr.io/github/slisovski/TwGeos/man/findTwilights.html) of the
#' package `TwGeos`.
#'
#' @param light a dataframe with columns \code{date} and \code{obs} that are the sequence of sample
#' times (as POSIXct) and light levels recorded by the tag.
#' @param threshold the light threshold that defines twilight. If not provided, it uses the first
#' light (i.e, `obs>0`).
#' @param shift_k shift of the middle of the night compared to 00:00 UTC (in seconds). If not
#' provided, it will take the middle of all nights.
#' @return A data.frame with columns `twilight` (date-time of twilights) and `rise` (logical)
#' @seealso [Vignette Light Map
#' ](https://raphaelnussbaumer.com/GeoPressureR/articles/light-map.html)
#' @examples
#' pam_data <- pam_read(
#'   pathname = system.file("extdata", package = "GeoPressureR"),
#'   crop_start = "2017-06-20", crop_end = "2018-05-02"
#' )
#' twl <- find_twilights(pam_data$light)
#' head(twl)
#' @export
find_twilights <- function(light, threshold = NA, shift_k = NA) {
  stopifnot(is.data.frame(light))
  stopifnot("date" %in% names(light))
  stopifnot(inherits(light$date, "POSIXt"))
  stopifnot("obs" %in% names(light))
  stopifnot(is.numeric(light$obs))

  if (is.na(threshold)) {
    threshold <- min(light$obs[light$obs > 0])
  }
  stopifnot(is.numeric(threshold))

  # add padding of time to center if night are not at 00:00 UTC
  if (is.na(shift_k)) {
    mat <- light2mat(light, shift_k = 0)
    res <- as.numeric(difftime(mat$date[2], mat$date[1], units = "secs"))
    l <- mat$obs >= threshold
    tmp <- rowMeans(l, na.rm = TRUE)
    shift_id <- round(sum(tmp * seq_len(dim(mat$obs)[1])) / sum(tmp))
    shift_k <- -(res * shift_id - 60 * 60 * 12)
  }

  mat <- light2mat(light, shift_k)

  # Compute exceed of light
  l <- mat$obs >= threshold
  # raster::image(l)

  # Find the first light
  id_sr <- apply(l, 2, which.max)
  if (any(id_sr == 1)) {
    warning(
      "There is likely a problem with the shiftK, ", sum(id_sr == 1),
      " twighlit set at midnight. shift_k=", shift_k
    )
  }
  id_sr_r <- id_sr + (seq_len(dim(l)[2]) - 1) * dim(l)[1]
  sr <- as.POSIXct(mat$date[id_sr_r], origin = "1970-01-01", tz = "UTC")

  id_ss <- dim(l)[1] - apply(l[nrow(l):1, ], 2, which.max)
  if (any(id_ss == 1)) {
    warning(
      "There is likely a problem with the shiftK, ", sum(id_ss == 1),
      " twighlit set at midnight. shift_k=", shift_k
    )
  }
  id_ss_s <- id_ss + (seq_len(dim(l)[2]) - 1) * dim(l)[1]
  ss <- as.POSIXct(mat$date[id_ss_s], origin = "1970-01-01", tz = "UTC")

  out <- data.frame(
    twilight = c(ss, sr),
    rise = c(!logical(length(ss)), logical(length(sr)))
  )
  # order by time
  return(out[order(out$twilight), ])
}


#' Convert light data in matrix format
#'
#' @param light a dataframe with columns \code{date} and \code{obs} that are the sequence of sample
#' times (as POSIXct) and light levels recorded by the tag.
#' @param shift_k shift of the middle of the night compared to 00:00 UTC (in seconds). If not
#' provided, will try to figure it out from the data
#' @return A dataframe with columns obs and date
#' @export
light2mat <- function(light, shift_k = 0) {
  stopifnot(is.data.frame(light))
  stopifnot("date" %in% names(light))
  stopifnot(inherits(light$date, "POSIXt"))
  stopifnot("obs" %in% names(light))
  stopifnot(is.numeric(light$obs))
  stopifnot(is.numeric(shift_k))

  res <- difftime(utils::tail(light$date, -1), utils::head(light$date, -1), units = "secs")
  stopifnot(length(unique(res)) == 1)
  res <- as.numeric(res[1])

  # Pad time to start and finish at 00:00
  date <- seq(
    from = as.POSIXct(format(light$date[1] + shift_k, "%Y-%m-%d"), tz = "UTC"),
    to = as.POSIXct(format(light$date[length(light$date)] + shift_k, "%Y-%m-%d"),
      tz = "UTC"
    ) + 60 * 60 * 24 - res,
    by = res
  )
  date <- date - shift_k

  obs <- rep(NA, length(date))
  obs[date %in% light$date] <- light$obs

  # reshape in matrix format
  mat <- list(
    obs = matrix(obs, nrow = 24 * 60 * 60 / res),
    date = matrix(date, nrow = 24 * 60 * 60 / res)
  )
  # raster::image(obs_r)
  mat$date <- as.POSIXct(mat$date, origin = "1970-01-01", tz = "UTC")

  return(mat)
}
