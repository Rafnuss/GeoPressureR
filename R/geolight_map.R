#' Compute likelihood map from twilight
#'
#' @description
#' This function estimates a likelihood map for each stationary period based on twilight data.
#' The function performs the following steps:
#'
#' 1. Perform a calibration on the known stationary period. See below for details
#' 2. Compute a likelihood map for each twilight using the calibration.
#' 3. Combine the likelihood maps of all twilights belonging to the same stationary periods with a
#' log-linear pooling. See [GeoPressureManual | Probability aggregation](
#' https://raphaelnussbaumer.com/GeoPressureManual/probability-aggregation.html)
#' for more information on probability aggregation using log-linear pooling.
#'
#' # Calibration
#'
#' Calibration requires to have a known position for a least one stationary periods. Use
#' `tag_set_map()` to define the known position.
#'
#' Instead of calibrating the twilight errors in terms of duration, we directly model the zenith
#' angle error. We use a kernel distribution to fit the zenith angle during the known stationary
#' period(s). The `twl_calib_adjust` parameter allows to manually adjust how smooth you want the
#' fit of the zenith angle to be. Because the zenith angle error model is fitted with data from the
#' calibration site only, and we are using it for all locations of the bird’s journey, it is safer
#' to assume a broader/smoother distribution.
#'
#' @param tag a GeoPressureR `tag` object.
#' @param twl_calib_adjust smoothing parameter for the kernel density (see [`stats::density()`]).
#' @param twl_llp log-linear pooling aggregation weight.
#' @param compute_known logical defining if the map(s) for known stationary period should be
#' estimated based on twilight or hard defined by the known location `stap$known_l**`
#' @param quiet logical to hide messages about the progress
#'
#' @return a `tag` with the likelihood of light as `tag$map_light`
#'
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   # Read geolocator data and build twilight
#'   tag <- tag_create("18LX", quiet = TRUE) |>
#'     tag_label(quiet = TRUE) |>
#'     tag_set_map(
#'       extent = c(-16, 23, 0, 50),
#'       scale = 10,
#'       known = data.frame(
#'         stap_id = 1,
#'         known_lon = 17.05,
#'         known_lat = 48.9
#'       )
#'     )
#'
#'   # Compute the twilight
#'   tag <- twilight_create(tag) |> twilight_label_read()
#'
#'   # Compute likelihood map
#'   tag <- geolight_map(tag, quiet = TRUE)
#' })
#'
#' plot(tag, type = "map_light")
#'
#'
#' # Calibration kernel fit can be retrieved from
#'
#' twl_calib <- tag$param$geolight_map$twl_calib
#'
#' library(ggplot2)
#'
#' x_lim <- range(twl_calib$x[twl_calib$y > .001 * max(twl_calib$y)])
#'
#' line_data <- data.frame(
#'   x = twl_calib$x,
#'   y = twl_calib$y / max(twl_calib$y) * max(twl_calib$hist_count)
#' )
#'
#' line_data <- line_data[line_data$x >= x_lim[1] & line_data$x <= x_lim[2], ]
#'
#' ggplot() +
#'   geom_bar(aes(x = twl_calib$hist_mids, y = twl_calib$hist_count),
#'     stat = "identity", fill = "lightblue", color = "blue",
#'     width = diff(twl_calib$hist_mids)[1]
#'   ) +
#'   geom_line(data = line_data, aes(x = x, y = y), color = "red", linewidth = 1) +
#'   labs(x = "Solor zenith angle", y = "Count of twilights") +
#'   theme_minimal() +
#'   xlim(x_lim) +
#'   theme(legend.position = "none")
#'
#' @family geolight
#' @export
geolight_map <- function(
  tag,
  twl_calib_adjust = 1.4,
  twl_llp = \(n) log(n) / n,
  compute_known = FALSE,
  quiet = FALSE
) {
  # Check tag
  tag_assert(tag, "setmap")
  tag_assert(tag, "twilight")

  twl <- tag$twilight

  # Warning if not labelled
  if (!("label" %in% names(twl))) {
    cli::cli_warn(c(
      "!" = "There are no {.field label} in {.var twilight}.",
      "i" = "We will assume that there are no discarded twilights.",
      ">" = "Make sure to label the twilight with {.fun twilight_label_read}."
    ))
    twl$label <- ""
  }

  # Compute a kernel density object containing the fit of the distribution of the twilights at the
  # known location
  twl_calib <- geolight_calibration(
    twl = twl,
    stap_known = tag$stap, # you can send all stap and the function will filter for those needed
    twl_calib_adjust = twl_calib_adjust
  )

  # Find index of twilight to compute: (1) no NA, (2) not discarded, (3)
  twl_id <- which(stats::complete.cases(twl) & twl$label != "discard")
  # Only select twilight that we are interested of: not known and/or not in flight
  if (!compute_known) {
    twl_id <- twl_id[
      twl$stap_id[twl_id] %in% tag$stap$stap_id[is.na(tag$stap$known_lat)]
    ]
  } else {
    twl_id <- twl_id[twl$stap_id[twl_id] %in% tag$stap$stap_id]
  }

  # compute the likelihood of observing the zenith angle of each twilight using the calibrated
  # error function for each grid cell.

  # compute the likelihood of observing the zenith angle of each twilight using the calibrated
  # error function for each grid cell

  pgz <- geolight_map_twilight(
    twl = twl[twl_id, ],
    extent = tag$param$tag_set_map$extent,
    scale = tag$param$tag_set_map$scale,
    twl_calib = twl_calib,
    quiet = quiet
  )

  # Group twilight by stap
  twl_id_stap_id <- split(seq_along(twl$stap_id[twl_id]), twl$stap_id[twl_id])

  # Compute the number of twilight per stap
  ntwl <- unlist(lapply(twl_id_stap_id, length))
  stopifnot(ntwl > 0)

  # Compute grid information
  g <- map_expand(tag$param$tag_set_map$extent, tag$param$tag_set_map$scale)

  # Initialize the likelihood list from stap to make sure all stap are present
  lk <- replicate(
    nrow(tag$stap),
    matrix(1, nrow = g$dim[1], ncol = g$dim[2]),
    simplify = FALSE
  )

  if (!quiet) {
    cli::cli_progress_bar(
      name = "Combine maps per stationary periods",
      total = sum(ntwl)
    )
  }
  for (i in seq_len(length(twl_id_stap_id))) {
    # find all twilight from this stap
    id <- twl_id_stap_id[[i]]

    # Combine with a Log-linear equation express in log
    if (length(id) > 1) {
      l <- exp(rowSums(
        twl_llp(length(id)) * log(pgz[, id] + .Machine$double.eps)
      ))
    } else if (length(id) == 1) {
      l <- pgz[, id]
    }
    lk[[as.numeric(names(twl_id_stap_id[i]))]] <- matrix(
      l,
      nrow = g$dim[1],
      ncol = g$dim[2]
    )
    if (!quiet) {
      cli::cli_progress_update(inc = ntwl[[i]])
    }
  }
  if (!quiet) {
    cli::cli_progress_done()
  }

  # Add known location
  if (!compute_known) {
    for (stap_id in tag$stap$stap_id[!is.na(tag$stap$known_lat)]) {
      # Initiate an empty map
      lk[[stap_id]] <- matrix(0, nrow = g$dim[1], ncol = g$dim[2])
      # Compute the index of the known position
      known_lon_id <- which.min(abs(tag$stap$known_lon[stap_id] - g$lon))
      known_lat_id <- which.min(abs(tag$stap$known_lat[stap_id] - g$lat))
      # Assign a likelihood of 1 for that position
      lk[[stap_id]][known_lat_id, known_lon_id] <- 1
    }
  }

  # Create map object
  tag$map_light <- map_create(
    data = lk,
    extent = tag$param$tag_set_map$extent,
    scale = tag$param$tag_set_map$scale,
    stap = tag$stap,
    id = tag$param$id,
    type = "light"
  )

  attr(twl_llp, "srcref") <- NULL
  attr(twl_llp, "srcfile") <- NULL
  environment(twl_llp) <- baseenv()

  # Add parameters
  tag$param$geolight_map <- list(
    twl_calib_adjust = twl_calib_adjust,
    twl_llp = twl_llp,
    compute_known = compute_known,
    twl_calib = twl_calib
  )

  return(tag)
}


#' Calibration of twilight based on known location
#'
#' @param twl A twilight data.frame as computed by twilight_create.
#' @param stap_known A stap data.frame with known location
#' @inheritParams geolight_map
#' @return A kernel calibration object
#' @seealso [`geolight_map()`]
#' @noRd
geolight_calibration <- function(
  twl,
  stap_known,
  twl_calib_adjust = formals(geolight_map)$twl_calib_adjust
) {
  assertthat::assert_that(is.numeric(twl_calib_adjust))
  assertthat::assert_that(all(
    c("known_lat", "known_lon", "start", "end") %in% names(stap_known)
  ))
  assertthat::assert_that(all(c("twilight", "stap_id") %in% names(twl)))

  # remove any staps without known
  stap_known <- stap_known[
    !is.na(stap_known$known_lat) & !is.na(stap_known$known_lon),
  ]

  if (nrow(stap_known) == 0) {
    cli::cli_abort(c(
      x = "There are no known location on which to calibrate in {.var stap_known}.",
      ">" = "Add a the calibration stationary period {.var known} with {.fun tag_set_map}."
    ))
  }

  # Remove NA value
  twl_clean <- stats::na.omit(twl)

  # Remove outlier
  if ("label" %in% names(twl_clean)) {
    twl_clean <- twl_clean[twl_clean$label != "discard", ]
  }

  if (nrow(twl_clean) == 0) {
    cli::cli_abort(c(
      x = "There are no twilights left after labeling."
    ))
  }

  # Calibrate the twilight in term of zenith angle with a kernel density.
  z_calib <- c()
  for (i in seq_len(nrow(stap_known))) {
    id <- twl_clean$twilight >= stap_known$start[i] &
      twl_clean$twilight <= stap_known$end[i]
    sun_calib <- geolight_solar(twl_clean$twilight[id])
    z_calib <- c(
      z_calib,
      geolight_refracted(geolight_zenith(
        sun_calib,
        stap_known$known_lon[i],
        stap_known$known_lat[i]
      ))
    )
  }

  # Compute the kernel density
  twl_calib <- stats::density(
    z_calib,
    adjust = twl_calib_adjust,
    from = 60,
    to = 120
  )

  # Compute the histogram
  hist_vals <- graphics::hist(z_calib, plot = FALSE)
  twl_calib$hist_count <- hist_vals$density * length(z_calib)
  twl_calib$hist_mids <- hist_vals$mids

  # Add the adjust parameter
  twl_calib$adjust <- twl_calib_adjust

  # return the twlight calibration object
  twl_calib
}


#' Compute the likelihood map of each twilight
#'
#' @param twl A twilight data.frame as computed by twilight_create.
#' @param twl_calib A kernel calibration object
#' @inheritParams tag_set_map
#' @inheritParams geolight_map
#' @return dsd
#' @seealso [`geolight_map()`]
#' @noRd
geolight_map_twilight <- function(
  twl,
  extent,
  scale,
  twl_calib,
  quiet = FALSE
) {
  # construct the grid of latitude and longitude on cell centred
  g <- map_expand(extent, scale)
  m <- expand.grid(lat = g$lat, lon = g$lon)
  ml <- split(m, seq_len(nrow(m)))

  # Compute the sun angle
  sun <- geolight_solar(twl$twilight)

  # Loop through each grid cell (location lat, lon) and compute the likelihood for all twilight
  if (!quiet) {
    pgz <- lapply(
      cli::cli_progress_along(
        ml,
        name = "Compute a likelihood map for each twilight"
      ),
      function(i) {
        z <- geolight_refracted(geolight_zenith(sun, ml[[i]]$lon, ml[[i]]$lat))
        stats::approx(twl_calib$x, twl_calib$y, z, yleft = 0, yright = 0)$y
      }
    )
  } else {
    pgz <- lapply(ml, function(i) {
      z <- geolight_refracted(geolight_zenith(sun, i$lon, i$lat))
      stats::approx(twl_calib$x, twl_calib$y, z, yleft = 0, yright = 0)$y
    })
  }

  # return the map
  do.call(rbind, pgz)
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
  eqctr <- sin(rad * m) *
    (1.914602 - jc * (0.004817 + 0.000014 * jc)) +
    sin(rad * 2 * m) * (0.019993 - 0.000101 * jc) +
    sin(rad * 3 * m) * 0.000289

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
  eqn_time <- 4 /
    rad *
    (y *
      sin(rad * 2 * l0) -
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
#' refraction see `GeoPressureR:::geolight_refracted()`.
#' @param sun List of solar time and declination computed by `geolight_solar`.
#' @param lon Vector of longitudes.
#' @param lat Vector latitudes.
#' @return A vector of solar zenith angles (degrees) for the given locations and times.
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
  cos_zenith <- (sin(rad * lat) *
    sun$sin_solar_dec +
    cos(rad * lat) * sun$cos_solar_dec * cos(rad * hour_angle))

  # Limit to [-1,1] [!!]
  cos_zenith[cos_zenith > 1] <- 1
  cos_zenith[cos_zenith < -1] <- -1

  # Ignore refraction correction
  acos(cos_zenith) / rad
}


#' Atmospheric refraction
#'
#' Adjust the solar zenith angle computed by `GeoPressureR:::geolight_zenith()` for the effect of
#' atmospheric refraction.
#'
#' @param zenith Zenith angle (degrees) to adjust.
#' @return Vector of zenith angles (degrees) adjusted for atmospheric refraction.
#' @noRd
geolight_refracted <- function(zenith) {
  rad <- pi / 180
  e <- 90 - zenith
  te <- tan((rad) * e)
  # Atmospheric Refraction [AF]
  r <- ifelse(
    e > 85,
    0,
    ifelse(
      e > 5,
      58.1 / te - 0.07 / te^3 + 0.000086 / te^5,
      ifelse(
        e > -0.575,
        1735 +
          e *
            (-518.2 +
              e *
                (103.4 + e * (-12.79 + e * 0.711))),
        -20.772 / te
      )
    )
  )
  # Corrected Zenith [90-AG]
  zenith - r / 3600
}
