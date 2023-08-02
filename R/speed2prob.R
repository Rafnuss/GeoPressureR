#' Probability of flight
#'
#' Converts a speed (airspeed or ground speed) to a probability using different parametric functions.
#'
#' @param speed airspeed or ground speed in km/h
#' @param movement a list of the movement model parameter defined with `graph_add_movement`
#'
#' @return Probability values corresponding to the speed provided.
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
#' @family movement
#' @export
speed2prob <- function(speed, movemement) {
  if (is.complex(speed)) {
    speed <- abs(speed)
  }
  assertthat::assert_that(is.numeric(speed))
  assertthat::assert_that(all(speed >= 0))


  # We use a normalization so that methods are comparable to each other.
  # The normalization is computed as the sum of probability with a 1km/h unit grid
  norm_speed <- pmax(seq(0, 150), movemement$low_speed_fix)

  speed <- pmax(speed, movemement$low_speed_fix)

  if (movemement$method == "gamma") {
    norm <- sum(stats::dgamma(norm_speed, shape = movemement$shape, scale = movemement$scale))
    prob <- stats::dgamma(speed, shape = movemement$shape, scale = movemement$scale) / norm
  } else if (movemement$method == "logis") {
    norm <- sum(stats::plogis(norm_speed, location = movemement$location, scale = movemement$scale, lower.tail = FALSE))
    prob <- stats::plogis(speed, location = movemement$location, scale = movemement$scale, lower.tail = FALSE) / norm
  } else if (movemement$method == "power") {
    # `speed2power` is defined in m/s (SI), but the rest of your code is using km/h. This is where
    # we need to convert.
    as <- speed * 1000 / 60 / 60

    # We normalize the probability computed by `power2prob`
    norm <- sum(movemement$power2prob(speed2power(norm_speed * 1000 / 60 / 60, movemement$bird)))

    prob <- movemement$power2prob(speed2power(as, movemement$bird)) / norm
  }

  return(prob)
}


#' Power curve
#'
#' Compute the mechanical power (W =J/s) required for a specific bird flying as at a given airspeed
#' in m/s. `bird` is created with [`bird_create()`].
#'
#' @param as airspeed in m/s
#' @param bird list of basic morphological trait necessary: mass, wing span, wing aspect ratio and
#'   body frontal area. It is best practice to create bird with [`bird_create()`].
#' @return mechanical power in Watt (or Joule/seconds) corresponding to the airspeed
#' @seealso [`bird_create()`], [`speed2prob()`]
#' @examples
#' bird <- bird_create("Acrocephalus arundinaceus")
#' airspeed <- seq(0, 30)
#' power <- speed2power(airspeed, bird)
#' plot(airspeed, power, xlab = "Airspeed [m/s]", ylab = "Mechanical Power [W]", type = "l")
#' @noRd
speed2power <- function(as, bird) {
  assertthat::assert_that(is.numeric(as))
  assertthat::assert_that(all(as >= 0))
  assertthat::assert_that(inherits(bird, "bird"))
  assertthat::assert_that(assertthat::has_name(bird, c(
    "mass", "wing_span", "body_frontal_area",
    "wing_aspect"
  )))

  # Constant of gravity [ms-2]
  g <- 9.80665
  # Air density
  rho <- 1.225

  # Induced power factor k=1.1-1.2 for aircraft and helicopter, ad k=1.04 in Spedding (1987a) (p.
  # 45).
  k <- 1.2
  # body drag coefficient (p. 51).[-]
  c_db <- 0.1
  c_pro <- 8.4

  # Induce power (eq 16 of Box 3.1) Pind is due to the active acceleration of mass flow in order to
  # produce a force opposing weight and drag
  p_ind <- 2 * k * (bird$mass * g)^2 / (as * pi * bird$wing_span^2 * rho)

  # Parasitic power (eq 3 of Box 3.2) (also called Body Power) due to drag on the body
  p_par <- rho * as^3 * bird$body_frontal_area * c_db / 2

  # Profile power due to the local drag on the wings
  p_am <- 1.05 * k^(3 / 4) * bird$mass^(3 / 2) * g^(3 / 2) *
    bird$body_frontal_area^(1 / 4) * c_db^(1 / 4) / rho^(1 / 2) /
    bird$wing_span^(3 / 2)
  p_pro <- c_pro / bird$wing_aspect * p_am

  # Total Mechanical Power (eq 1 of Box 3.4)
  p_mech <- p_ind + p_par + p_pro

  return(p_mech)
}
