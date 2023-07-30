#' Create bird flight traits
#'
#' @description
#' This function returns a list with the four morphological information necessary to construct the
#' power curve: mass, wing span, wing aspect ratio, and body frontal area.
#'
#' When any of these variables are missing, we query the
#' \href{https://doi.org/10.6084/m9.figshare.16586228.v5}{AVONET database} using the scientific
#' name from \href{https://www.birds.cornell.edu/clementschecklist/}{the Clements Checklist}.
#'
#' @param species_name Scientific name of the species
#' @param mass Mass of the bird in kilograms.
#' @param wing_span Wing span in meters.
#' @param wing_aspect Wing aspect ratio (no unit).
#' @param wing_area Wing area in meter square.
#' @param body_frontal_area Body frontal area in meter square.
#' @return List containing mass, wing span, wing aspect ratio, and body frontal area.
#' @examples
#' # Using AVONET dataset
#' bird_create("Acrocephalus arundinaceus")
#' # Using AVONET dataset + custom values
#' bird_create("Acrocephalus arundinaceus", wing_aspect = 8)
#' # Import your own bird. You will need mass, and at least two of wing_span,
#' # wing_aspect or wing_area.
#' bird_create("custum_bird", mass = 1, wing_span = 1, wing_aspect = 4)
#' @family bird
#' @export
bird_create <- function(species_name,
                        mass = NULL,
                        wing_span = NULL,
                        wing_aspect = NULL,
                        wing_area = NULL,
                        body_frontal_area = NULL) {
  if (is.null(mass) || (is.null(wing_aspect) + is.null(wing_area) + is.null(wing_span) > 1)) {
    # Mass, wing length and secondary length are retrieve from the AVONET
    sp_id <- grep(species_name, avonet$species, ignore.case = TRUE)
    if (length(sp_id) == 0) {
      cli::cli_abort("No match for {.val species_name}. Please use the exact scientific name.
                      Closest matches are:
                      {avonet$species[agrep(species_name, avonet$species, ignore.case = TRUE)]}")
    } else if (length(sp_id) > 1) {
      cli::cli_abort("Multiple match for {.val species_name}. Please use the exact scientific name.
      {avonet$species[sp_id]}")
    }
    b <- avonet[sp_id, ]
    b$mass <- b$mass / 1000 # g -> kg
    b$wing_length <- b$wing_length / 1000 # cm -> m
    b$secondary <- b$secondary / 1000 # cm -> m
  }

  # Mass
  if (is.null(mass)) {
    mass <- b$mass
  }

  # Body frontal area
  if (is.null(body_frontal_area)) {
    # Assuming that the bird is a passerine, [Hedenström and Rosén (2003)]
    # (https://doi.org/10.1034/j.1600-048X.2003.03145.x) is used with
    body_frontal_area <- 0.0129 * mass^(0.614)
    # In case of non-passrine, Pennycuick et al. (1988) could be used body_frontal_area =
    # 0.00813*mass^(0.666)
  }

  # Combination of wing area, span and aspect ratio
  if (!is.null(wing_area) && is.null(wing_span) && !is.null(wing_aspect)) {
    wing_span <- sqrt(wing_aspect * wing_area)
  } else if (!is.null(wing_area) && !is.null(wing_span) && is.null(wing_aspect)) {
    wing_aspect <- wing_span^2 / wing_area
  }

  # Wing span alone
  if (is.null(wing_span)) {
    # From [Duncan (1990)](https://doi.org/10.2307/4088014)
    wing_span <- 2 * (1.32 * b$wing_length * 1000 - 4.80) / 1000
  }

  # Wing aspect ratio
  if (is.null(wing_aspect)) {
    if (!is.null(wing_area)) {
      wing_aspect <- wing_span^2 / wing_area
    } else {
      # assume that mean chord length is equal to half of the secondary
      chord <- b$secondary / 2
      wing_aspect <- wing_span / chord
    }
  }

  # Final check of the input and return the list
  assertthat::assert_that(is.character(species_name))
  assertthat::assert_that(is.numeric(mass))
  assertthat::assert_that(mass > 0 & mass < 10)
  assertthat::assert_that(is.numeric(body_frontal_area))
  assertthat::assert_that(body_frontal_area > 0 & body_frontal_area < 1)
  assertthat::assert_that(is.numeric(wing_span))
  assertthat::assert_that(wing_span > 0 & wing_span < 10)
  assertthat::assert_that(is.numeric(wing_aspect))
  assertthat::assert_that(wing_aspect > 1 & wing_aspect < 100)

  return(structure(list(
    species_name = species_name,
    mass = mass,
    body_frontal_area = body_frontal_area,
    wing_span = wing_span,
    wing_aspect = wing_aspect
  ), class = "bird"))
}
