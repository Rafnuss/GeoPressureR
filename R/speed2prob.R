#' Probability of flight
#'
#' Converts a speed (airspeed or ground speed) to a probability using different parametric functions.
#'
#' @param speed airspeed or ground speed in km/h
#' @param method method used to convert the speed to probability ("gamma", "logis" or "power")
#' @param shape parameter of the gamma distribution
#' @param scale  parameter of the gamma and logistic distribution
#' @param location parameter for the logistic distribution
#' @param bird list of basic morphological traits necessary: mass, wing span, wing aspect ratio, and
#'   body frontal area. It is good practice to create a bird with [`bird_create()`].
#' @param power2prob function taking power as a single argument and returning a probability
#' @param low_speed_fix speed below which the probability remains the same. This parameter is used
#'   to allow short flights covering small distances.
#' @return Probability values corresponding to the speed provided.
#' @seealso [`bird_create()`]
#' @examples
#' speed <- seq(1, 120)
#' low_speed_fix <- 20 # minimum speed allowed
#' prob <- speed2prob(speed,
#'   method = "gamma", shape = 7, scale = 7,
#'   low_speed_fix = low_speed_fix
#' )
#' plot(speed, prob,
#'   type = "l", xlab = "Groundspeed [km/h]", ylab =
#'     "Probability"
#' )
#' abline(v = low_speed_fix)
#' bird <- bird_create("Acrocephalus arundinaceus")
#' prob <- speed2prob(speed, method = "power", bird = bird)
#' plot(speed, prob, type = "l", xlab = "Airspeed [km/h]", ylab = "Probability")
#' @export
speed2prob <- function(speed,
                       method = "gamma",
                       shape = 7,
                       scale = 7,
                       location = 40,
                       bird = NA,
                       power2prob = \(power) (1 / power)^3,
                       low_speed_fix = 15) {
  if (is.complex(speed)) {
    speed <- abs(speed)
  }
  assertthat::assert_that(is.numeric(speed))
  assertthat::assert_that(all(speed >= 0))
  assertthat::assert_that(is.character(method))
  assertthat::assert_that(any(c("gamma", "power", "logis") == method))
  assertthat::assert_that(is.function(power2prob))

  # We use a normalization so that methods are comparable to each other.
  # The normalization is computed as the sum of probability with a 1km/h unit grid
  norm_speed <- pmax(seq(0, 150), low_speed_fix)

  speed <- pmax(speed, low_speed_fix)

  if (method == "gamma") {
    norm <- sum(stats::dgamma(norm_speed, shape = shape, scale = scale))
    prob <- stats::dgamma(speed, shape = shape, scale = scale) / norm
  } else if (method == "logis") {
    norm <- sum(stats::plogis(norm_speed, location = location, scale = scale, lower.tail = FALSE))
    prob <- stats::plogis(speed, location = location, scale = scale, lower.tail = FALSE) / norm
  } else if (method == "power") {
    # Power method need to have a bird flight.
    assertthat::assert_that(is.list(bird))

    # `speed2power` is defined in m/s (SI), but the rest of your code is using km/h. This is where
    # we need to convert.
    as <- speed * 1000 / 60 / 60

    # We normalize the probability computed by `power2prob`
    norm <- sum(power2prob(speed2power(norm_speed * 1000 / 60 / 60, bird)))

    prob <- power2prob(speed2power(as, bird)) / norm
  }

  return(prob)
}
