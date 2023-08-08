#' Compute likelihood map from twilight
#'
#' @description
#' This functions estimate a likelihood map for each stationary period from light and twilight data.
#' The functions performs the following steps:
#'
#' 1. Perform a calibration on the known stationary period.
#' 2. Compute a likelihood map for each twilight using the calibration
#' 3. Combine all likelihood maps of the same calibration period using the stationary period dates
#'
#' # Calibration
#'
#' Calibration requires to have a known position for a least one stationary periods. Use
#' [`tag_setmap`] to define the known position.
#'
#' Instead of calibrating the twilight errors in terms of duration, we directly model the zenith
#' angle error. We use a kernel distribution to fit the zenith angle during the known stationary
#' period(s). The `twl_calib_adjust` parameter allows to manually adjust how smooth you want the
#' fit of the zenith angle to be. Because the zenith angle error model is fitted with data from the
#' calibration site only, and we are using it for all locations of the bird’s journey, it is safer
#' to assume a broader/smoother distribution.
#'
#' # Log-linear pooling of the twilight likelihood map
#'
#' The twilight maps are aggregated by stationary period according to the date and time defined in
#' `stap` and twilight. See [GeoPressureManual | Probability aggregation](
#' https://raphaelnussbaumer.com/GeoPressureManual/probability-aggregation.html#probability-aggregation-1)
#' for more information on probability aggregation using log-linear pooling.
#'
#' @param tag A GeoPressureR `tag` object with setmap status.
#' @param twl_calib_adjust Smoothing parameter for the kernel density (see [`stats::kernel()`]).
#' @param twl_llp Log-linear pooling aggregation weight.
#' @param compute_known Logical defining if the map(s) for known stationary period should be
#' estimated based on twilight or hard defined by the known location `stap$known_l**`
#' @return a `tag` with the likelihood of light as `tag$map_light`
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#' # Read geolocator data and build twilight
#' tag <- tag_create("18LX", quiet = T) |>
#'   tag_label(quiet = T) |>
#'   tag_setmap(
#'     extent = c(-16, 23, 0, 50),
#'     scale = 10,
#'     known = data.frame(
#'       stap_id = 1,
#'       known_lon = 17.05,
#'       known_lat = 48.9
#'     )
#'   )
#'
#' # Compute the twilight
#' tag <- twilight_create(tag) |> twilight_label_read()
#'
#' # Compute likelihood map
#' tag <- geolight_map(tag)
#'
#' plot(tag, type = "map_light")
#' @export
geolight_map <- function(tag,
                         twl_calib_adjust = 1.4,
                         twl_llp = \(n) 0.1,
                         compute_known = FALSE) {
  # Check tag
  tag_assert(tag, "setmap")

  # extract for convenience
  stap <- tag$stap

  if (all(is.na(stap$known_lat))) {
    cli::cli_abort(c(
      x = "There are no known location on which to calibrate in {.var stap$known_lat}.",
      ">" = "Add a the calibration stationary period {.var known} with {.fun tag_setmap}."
    ))
  }

  # Check twilight
  tag_assert(tag, "twilight")

  # extract for convenience
  twl <- tag$twilight

  # Add stap_id if missing
  if (!("stap_id" %in% names(twl))) {
    tmp <- mapply(function(start, end) {
      start <= twl$twilight & twl$twilight <= end
    }, stap$start, stap$end)
    tmp <- which(tmp, arr.ind = TRUE)
    twl$stap_id <- 0
    twl$stap_id[tmp[, 1]] <- tmp[, 2]
  }

  # check other
  assertthat::assert_that(is.numeric(twl_calib_adjust))
  assertthat::assert_that(is.function(twl_llp))

  # Check if labeled
  if (!("label" %in% names(twl))) {
    cli::cli_abort(c(
      x = "There are no {.field label} in {.var tag$twilight}.",
      ">" = "Make sure to label the twilight with {.fun twilight_label_read}."
    ))
  }

  # Remove outlier
  twl_clean <- twl[twl$label != "discard", ]

  # Calibrate the twilight in term of zenith angle with a kernel density.
  z_calib <- c()
  for (istap in which(!is.na(stap$known_lat))) {
    sun_calib <- geolight_solar(twl_clean$twilight[twl_clean$stap_id == istap])
    z_calib <- c(
      z_calib,
      geolight_refracted(geolight_zenith(
        sun_calib,
        stap$known_lon[istap],
        stap$known_lat[istap]
      ))
    )
  }
  twl_calib <- stats::density(z_calib, adjust = twl_calib_adjust, from = 60, to = 120)

  # compute the likelihood of observing the zenith angle of each twilight using the calibrated
  # error function for each grid cell.

  # Only select twilight that we are interested of: not known and/or not in flight (stap_id == 0)
  if (compute_known) {
    twl_clean_comp <- twl_clean[twl_clean$stap_id %in% stap$stap_id[is.na(stap$known_lat)], ]
  } else {
    twl_clean_comp <- twl_clean[twl_clean$stap_id %in% stap$stap_id, ]
  }

  # Compute the sun angle
  sun <- geolight_solar(twl_clean_comp$twilight)

  # Get grid information
  g <- map_expand(tag$param$extent, tag$param$scale)

  # construct the grid of latitude and longitude on cell centered
  m <- expand.grid(lat = g$lat, lon = g$lon)
  ml <- split(m, seq(nrow(m)))

  # Loop through each grid cell (location lat, lon) and compute the likelihood for all twilight
  pgz <- lapply(cli::cli_progress_along(ml, name = "Compute a map for each twilight"), function(i) {
    z <- geolight_refracted(geolight_zenith(sun, ml[[i]]$lon, ml[[i]]$lat))
    stats::approx(twl_calib$x, twl_calib$y, z, yleft = 0, yright = 0)$y
  })
  pgz <- do.call(rbind, pgz)


  # Group twilight by stap
  twl_id_stap_id <- split(seq_along(twl_clean_comp$stap_id), twl_clean_comp$stap_id)

  # Compute the number of twilight per stap
  ntwl <- unlist(lapply(twl_id_stap_id, length))
  stopifnot(ntwl > 0)

  # Initialize the likelihood list from stap to make sure all stap are present
  lk <- replicate(nrow(stap), matrix(1, nrow = g$dim[1], ncol = g$dim[2]), simplify = FALSE)

  cli::cli_progress_bar(name = "Combine maps per stationary periods", total = sum(ntwl))
  for (i in seq_len(length(twl_id_stap_id))) {
    # find all twilight from this stap
    id <- twl_id_stap_id[[i]]

    # Combine with a Log-linear equation express in log
    if (length(id) > 1) {
      l <- exp(rowSums(twl_llp(length(id)) * log(pgz[, id])))
    } else if (length(id) == 1) {
      l <- pgz[, id]
    }
    lk[[as.numeric(names(twl_id_stap_id[i]))]] <- matrix(l, nrow = g$dim[1], ncol = g$dim[2])
    cli::cli_progress_update(inc = ntwl[[i]])
  }
  cli::cli_progress_done()

  # Add known location
  if (!compute_known) {
    for (stap_id in stap$stap_id[!is.na(stap$known_lat)]) {
      # Initiate an empty map
      lk[[stap_id]] <- matrix(0, nrow = g$dim[1], ncol = g$dim[2])
      # Compute the index of the known position
      known_lon_id <- which.min(abs(stap$known_lon[stap_id] - g$lon))
      known_lat_id <- which.min(abs(stap$known_lat[stap_id] - g$lat))
      # Assign a likelihood of 1 for that position
      lk[[stap_id]][known_lat_id, known_lon_id] <- 1
    }
  }

  # Create map object
  tag$map_light <- map_create(
    data = lk,
    extent = tag$param$extent,
    scale = tag$param$scale,
    stap = tag$stap,
    id = tag$param$id,
    type = "light"
  )

  # Add parameters
  tag$param$twl_calib_adjust <- twl_calib_adjust
  tag$param$twl_llp <- twl_llp
  tag$param$twl_calib <- twl_calib

  return(tag)
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
