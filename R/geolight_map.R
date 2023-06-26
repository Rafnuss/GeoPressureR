#' Compute likelihood map from twilight
#'
#' @description
#' This functions uses a `geostap` and a data.frame of twilight to estimate a likelihood map
#' for each stationary period. The functions performs the following steps:
#'
#' 1. Perform a calibration on the known stationary period `geostap$stap$known_l**`.
#' 2. Compute a likelihood map for each twilight using the calibration
#' 3. Combine all likelihood maps of the same calibration period using the stationary period dates
#'
#' # Calibration
#'
#' Instead of calibrating the twilight errors in terms of duration, we directly model the zenith
#' angle error.
#' The `twl_calib_adjust` parameter allows to manually set how smooth you want the fit to be.
#' Because the zenith angle error model is fitted with data from the calibration site only, and
#' we are using it for all locations of the bird’s journey, it is safer to assume a broader/smoother
#' distribution.
#'
#' # Log-linear pooling of the twilight likelihood map
#'
#' The twilight maps are aggregated by stationary period according to the date and time defined in
#' `tag$stap` and twilight. See [GeoPressureManual | Probability aggregation](
#' https://raphaelnussbaumer.com/GeoPressureManual/probability-aggregation.html#probability-aggregation-1)
#' for more information on probability aggregation using log-linear pooling.
#'
#' @param geostap Data
#' @param twilight A data.frame with columns `twilight` (date-time of twilights) and `discard`
#' (see [`twilight_create()`])
#' @param twl_calib_adjust Smoothing parameter for the kernel density (see [`stats::kernel()`]).
#' @param twl_llp Log-linear pooling aggregation weight.
#' @return a `geostap` with the likelihood of light as `map_light`
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#' # Read geolocator data and build twilight
#' tag <- tag_create("18LX") |> tag_label()
#' tag <- twilight_create(tag) |> twilight_label_read()
#' # Initiate the geostap structure
#' geostap <- geostap_create(tag,
#'   extent = c(-16, 23, 0, 50),
#'   scale = 10,
#'   known = data.frame(
#'     stap_id = 1,
#'     known_lon = 17.05,
#'     known_lat = 48.9
#'   )
#' )
#'
#' # Compute likelihood map
#' geostap <- geolight_map(
#'   geostap,
#'   tag$twilight
#' )
#'
#' terra::plot(
#'   terra::rast(geostap$map_light[[1]],
#'     extent = geostap$extent
#'   ),
#'   main = "Likelihood"
#' )
#' @export
geolight_map <- function(geostap,
                         twilight,
                         twl_calib_adjust = 1.4,
                         twl_llp = \(n) 0.1) {
  # Check geostap
  assertthat::assert_that(is.list(geostap))
  assertthat::assert_that(assertthat::has_name(geostap, "stap"))
  assertthat::assert_that(assertthat::has_name(geostap, "scale"))
  assertthat::assert_that(assertthat::has_name(geostap, "extent"))
  assertthat::assert_that(is.data.frame(geostap$stap))
  assertthat::assert_that(assertthat::has_name(geostap$stap, "stap_id"))
  assertthat::assert_that(assertthat::has_name(geostap$stap, "known_lat"))
  assertthat::assert_that(assertthat::has_name(geostap$stap, "known_lon"))
  assertthat::assert_that(assertthat::has_name(geostap$stap, "start"))
  assertthat::assert_that(assertthat::has_name(geostap$stap, "end"))
  if (all(is.na(geostap$stap$known_lat))) {
    cli::cli_abort(c(
      x = "There are no known location on which to calibrate in {.var geostap$stap$known_lat}.",
      i = "Add a the calibration stationary period {.var known} when creating {.var geostap} \\
      with {.fun geostap_create}."
    ))
  }
  # Check twilight
  assertthat::assert_that(is.data.frame(twilight))
  assertthat::assert_that(assertthat::has_name(twilight, c("twilight", "label")))
  # Add stap_id if missing
  if ("stap_id" %in% names(twilight)) {
    tmp <- mapply(function(start, end) {
      start <= twilight$twilight & twilight$twilight <= end
    }, geostap$stap$start, geostap$stap$end)
    tmp <- which(tmp, arr.ind = TRUE)
    twilight$stap_id <- 0
    twilight$stap_id[tmp[, 1]] <- tmp[, 2]
  }
  assertthat::assert_that(inherits(twilight$twilight, "POSIXt"))
  assertthat::assert_that(is.character(twilight$label))
  # check other
  assertthat::assert_that(is.numeric(twl_calib_adjust))
  assertthat::assert_that(is.function(twl_llp))

  # Remove outlier
  twilight_clean <- twilight[twilight$label != "discard", ]

  # Calibrate the twilight in term of zenith angle with a kernel density.
  z_calib <- c()
  for (istap in which(!is.na(geostap$stap$known_lat))) {
    sun_calib <- geolight_solar(twilight$twilight[twilight$stap_id == istap])
    z_calib <- c(
      z_calib,
      geolight_refracted(geolight_zenith(
        sun_calib,
        geostap$stap$known_lon[istap],
        geostap$stap$known_lat[istap]
      ))
    )
  }
  twl_calib <- stats::density(z_calib, adjust = twl_calib_adjust, from = 60, to = 120)

  # compute the likelihood of observing the zenith angle of each twilight using the calibrated
  # error function for each grid cell.

  # Compute the sun angle
  sun <- geolight_solar(twilight_clean$twilight)

  # construct the grid of latitude and longitude on cell centered
  # call new function
  g <- geo_expand(geostap$extent, geostap$scale)
  m <- data.frame(
    lon = rep(g$lon, each = g$dim[1]),
    lat = rep(g$lat, times = g$dim[2]),
    likelihood = 1
  )

  pgz <- apply(m, 1, function(x) {
    z <- geolight_refracted(geolight_zenith(sun, x[1], x[2]))
    stats::approx(twl_calib$x, twl_calib$y, z, yleft = 0, yright = 0)$y
  })

  # Initialize the likelihood list from stap to make sure all stap are present
  lk <- vector("list", nrow(geostap$stap))

  for (stap_id in seq_len(length(lk))) {
    # find all twilights from this stap
    id <- twilight_clean$stap_id == stap_id
    if (sum(id) > 1) {
      l <- exp(colSums(twl_llp(sum(id)) * log(pgz[id, ]))) # Log-linear equation express in log
    } else if (sum(id) == 1) {
      l <- pgz[id, ]
    } else {
      l <- 1
    }
    l <- matrix(l, nrow = g$dim[1], ncol = g$dim[2])
    lk[[stap_id]] <- l
  }

  # Add known location
  if (FALSE) {
    for (stap_id in which(!is.na(geostap$stap$known_lat))) {
      # Initiate an empty map
      lk[[stap_id]] <- matrix(0, nrow = g$dim[1], ncol = g$dim[2])
      # Compute the index of the known position
      known_lon_id <- which.min(abs(geostap$stap$known_lon[stap_id] - g$lon))
      known_lat_id <- which.min(abs(geostap$stap$known_lat[stap_id] - g$lat))
      # Assign a likelihood of 1 for that position
      lk[[stap_id]][known_lat_id, known_lon_id] <- 1
    }
  }

  geostap$map_light <- lk

  # Add parameters
  geostap$param$twl_calib_adjust <- twl_calib_adjust
  geostap$param$twl_llp <- twl_llp
  geostap$param$twl_calib <- twl_calib

  return(geostap)
}




#' Solar time and declination
#'
#' Calculate solar time, the equation of time and the sine and cosine of the solar declination.
#' These are calculated using the same methods as \url{https://gml.noaa.gov/grad/solcalc/}.
#'
#' @param date Vector of POSIXct times.
#' @return List containing the following vectors.
#' - `solar_time` solar time (degrees)
#' - `eqn_time` equation of time (minutes of time)
#' - `sin_solar_dec` sine of the solar declination
#' - `cos_solar_dec` cosine of the solar declination
#' @seealso [`geolight_zenith()`]
#' @examples
#' # Current solar time
#' GeoPressureR::geolight_solar(Sys.time())
#' @noRd
geolight_solar <- function(date) {
  rad <- pi / 180

  # Time as Julian day (R form)
  jd <- as.numeric(date) / 86400.0 + 2440587.5

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
#' s <- GeoPressureR::geolight_solar(as.POSIXct("2000-05-01 12:00:00", "EST"))
#' GeoPressureR::geolight_zenith(s, lon, lat)
#' @noRd
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
#' @noRd
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
