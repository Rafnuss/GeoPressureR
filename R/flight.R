#' Construct bird morphology
#'
#' This function return a list with the four morphological information necessary to construct the
#' power curve: mass, wing span, wing aspect ratio and body frontal area.
#'
#' When any of these variables are missing, we query the AVONET database]
#' (https://doi.org/10.6084/m9.figshare.16586228.v5) using the sciencific name from [the Clements
#' Checklist](https://www.birds.cornell.edu/clementschecklist/).
#'
#' @param species_name scientific name of the species
#' @param mass in kilogram
#' @param wing_span denoted B in meter
#' @param wing_aspect wing aspect ratio (no unit)
#' @param wing_area in meter square
#' @param body_frontal_area in meter square
#' @return list containing mass, wing span, wing aspect ratio and body frontal area.
#' @examples
#' # Using AVONET dataset
#' flight_bird("Acrocephalus arundinaceus")
#' # Using AVONET dataset + custom values
#' flight_bird("Acrocephalus arundinaceus", wing_aspect = 8)
#' # Import your own bird. You will need mass, and at least two of wing_span,
#' # wing_aspect or wing_area.
#' flight_bird("custum_bird", mass = 1, wing_span = 1, wing_aspect = 4)
#' @export
flight_bird <- function(species_name,
                        mass = NA,
                        wing_span = NA,
                        wing_aspect = NA,
                        wing_area = NA,
                        body_frontal_area = NA) {
  if (is.na(mass) | (is.na(wing_aspect) + is.na(wing_area) +
    is.na(wing_span) > 1)) {
    # Mass, wing length and secondary length are retrived from the AVONET
    avonet <- utils::read.csv(system.file("extdata", "avonet_clements.csv",
      package = "GeoPressureR"
    ))

    sp_id <- grep(species_name, avonet$species, ignore.case = TRUE)
    if (length(sp_id) == 0) {
      tmp <- print(avonet[agrep(species_name,
        avonet$species,
        ignore.case = TRUE
      ), ])
      stop(
        "No match for '", species_name,
        "'. Please use the exact name. \nClosest matches are: \n",
        paste(utils::capture.output(print(tmp)), collapse = "\n")
      )
    } else if (length(sp_id) > 1) {
      tmp <- print(avonet[sp_id, ])
      stop(
        "Multiple match for '", species_name,
        "'. Please use the exact name. \n",
        paste(utils::capture.output(print(tmp)), collapse = "\n")
      )
    }
    b <- avonet[sp_id, ]
    b$mass <- b$mass / 1000 # g -> kg
    b$wing_length <- b$wing_length / 1000 # cm -> m
    b$secondary <- b$secondary / 1000 # cm -> m
  }

  # Mass
  if (is.na(mass)) {
    mass <- b$mass
  }

  # Body frontal qrea
  if (is.na(body_frontal_area)) {
    # Assuming that the bird is a passerine, [Hedenström and Rosén (2003)]
    # (https://doi.org/10.1034/j.1600-048X.2003.03145.x) is used with
    body_frontal_area <- 0.0129 * mass^(0.614)
    # In case of non-passrine, Pennycuick et al. (1988) could be used body_frontal_area =
    # 0.00813*mass^(0.666)
  }

  # Combinaison of wing area, span and aspect ratio
  if (!is.na(wing_area) & is.na(wing_span) & !is.na(wing_aspect)) {
    wing_span <- sqrt(wing_aspect * wing_area)
  } else if (!is.na(wing_area) & !is.na(wing_span) & is.na(wing_aspect)) {
    wing_aspect <- wing_span^2 / wing_area
  }

  # Wing span alone
  if (is.na(wing_span)) {
    # From [Duncan (1990)](https://doi.org/10.2307/4088014)
    wing_span <- 2 * (1.32 * b$wing_length * 1000 - 4.80) / 1000
  }

  # Wing aspect ratio
  if (is.na(wing_aspect)) {
    if (!is.na(wing_area)) {
      wing_aspect <- wing_span^2 / wing_area
    } else {
      # assume that mean chord length is equal to half of the secondary
      chord <- b$secondary / 2
      wing_aspect <- wing_span / chord
    }
  }

  # Final check of the input and return the list
  stopifnot(is.character(species_name))
  stopifnot(is.numeric(mass))
  stopifnot(mass > 0 & mass < 10)
  stopifnot(is.numeric(body_frontal_area))
  stopifnot(body_frontal_area > 0 & body_frontal_area < 1)
  stopifnot(is.numeric(wing_span))
  stopifnot(wing_span > 0 & wing_span < 10)
  stopifnot(is.numeric(wing_aspect))
  stopifnot(wing_aspect > 1 & wing_aspect < 100)
  return(list(
    species_name = species_name,
    mass = mass,
    body_frontal_area = body_frontal_area,
    wing_span = wing_span,
    wing_aspect = wing_aspect
  ))
}




#' Power curve
#'
#' Compute the mechanical power required for a specific bird flying as at a given airspeed in m/s.
#' `bird` (created with `flight_bird()`)
#'
#' @param bird list of basic morphological trait necessary: mass, wing span, wing aspect ratio and
#'   body frontal area. It is best practice to create bird with `flight_bird()`.
#' @param as airspeed in m/s
#' @return mechanical power in Watt (or Joule/seconds) corresponding to the airspeed
#' @examples
#' bird <- flight_bird("Acrocephalus arundinaceus")
#' airspeed <- seq(0, 30)
#' power <- flight_power(airspeed, bird)
#' plot(airspeed, power, xlab = "Airspeed [m/s]", ylab = "Mechanical Power [W]")
#' @export
flight_power <- function(as, bird) {
  stopifnot(is.numeric(as))
  stopifnot(as >= 0)
  stopifnot(is.list(bird))
  stopifnot(c("mass", "wing_span", "body_frontal_area", "wing_aspect")
  %in% names(bird))


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



#' Movement model
#'
#' Compute the mechanical power required for a specific bird flying as at a given airspeed in km/h.
#' `bird` (created with `flight_bird()`)
#'
#' @param speed airspeed or groundspeed in km/h
#' @param method method used to convert the speed to probability ("gamma" or "power")
#' @param shape parameter of the gamma distribution
#' @param scale  parameter of the gamma distribution
#' @param bird list of basic morphological trait necessary: mass, wing span, wing aspect ratio and
#'   body frontal area. It is best practice to create bird with `flight_bird()`.
#' @param fun_power function taking power as a single argument and returning a probability
#' @param low_speed_fix speed below which the probability remains the same. This parameter is used
#'   to allow short flight covering small distance.
#' @return Probability values corresponding to the speed provided
#' @examples
#' speed <- seq(1, 120)
#' low_speed_fix <- 20 # minimum speed allowed
#' prob <- flight_prob(speed,
#'   method = "gamma", shape = 7, scale = 7,
#'   low_speed_fix = low_speed_fix
#' )
#' plot(speed, prob,
#'   type = "l", xlab = "Groundspeed [km/h]", ylab =
#'     "Probability"
#' )
#' abline(v = low_speed_fix)
#' bird <- flight_bird("Acrocephalus arundinaceus")
#' prob <- flight_prob(speed, method = "power", bird = bird)
#' plot(speed, prob, type = "l", xlab = "Airspeed [km/h]", ylab = "Probability")
#' @export
flight_prob <- function(speed,
                        method = "gamma",
                        shape = 7,
                        scale = 7,
                        bird = NA,
                        fun_power = function(power) {
                          (1 / power)^3
                        },
                        low_speed_fix = 15) {
  if (is.complex(speed)) {
    speed <- abs(speed)
  }
  stopifnot(is.numeric(speed))
  stopifnot(speed >= 0)
  stopifnot(is.character(method))
  stopifnot(any(c("gamma", "power") == method))
  stopifnot(is.function(fun_power))

  speed <- pmax(speed, low_speed_fix)

  if (method == "gamma") {
    return(stats::dgamma(speed, shape, 1 / scale))
  } else if (method == "power") {
    # `flight_power` is defined in m/s (SI), but the rest of your code is using km/h. This is where
    # we need to convert.
    as <- speed * 1000 / 60 / 60

    # We normalize the probability computed by `fun_power` so that it is comparable to the other
    # method. The normalization is computed as the sum of probability with a 1km/h unit grid
    norm <- sum(fun_power(flight_power(pmax(seq(0, 150), low_speed_fix) * 1000
      / 60 / 60, bird)))

    return(fun_power(flight_power(as, bird)) / norm)
  }
}
