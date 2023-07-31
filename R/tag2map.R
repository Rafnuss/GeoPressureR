#' Return a likelihood map from a `tag`
#'
#'
#' @param tag A GeoPressureR `tag` object.
#' @param likelihood Field of the `tag` list containing the likelihood map (character). Possible
#' value are `map_pressure`, `map_light`, `map_pressure_mse`, `map_pressure_mse`,
#' `map_pressure_mse`, `mask_water`. Default `NA` is to take the product of `map_pressure` and
#' `map_light`, or if not available, taking the first of the possible values.
#'
#' @return likelihood map
#' @export
tag2map <- function(tag, likelihood = NULL) {
  likelihood <- tag2likelihood(tag, likelihood)

  map <- tag[[likelihood[1]]]

  # Deal with multiple likelihood
  if (length(likelihood) > 1) {
    fn <- \(p, l) {
      if (is.null(p) | is.null(l)) {
        return(NULL)
      } else {
        return(p * l)
      }
    }

    for (i in seq(2, length(likelihood))) {
      map <- mapply(fn, map, tag[[likelihood[i]]], SIMPLIFY = FALSE)
    }
  }

  # Add attribute
  attr(map, "id") <- tag$param$id
  attr(map, "extent") <- tag$extent
  attr(map, "scale") <- tag$scale
  attr(map, "stap") <- tag$stap

  return(map)
}

#' Return a valid likelihood map name
#
#' @param A GeoPressureR `tag` object.
#' @inheritParams tag2map
#'
#' @return likelihood map name
tag2likelihood <- function(tag, likelihood = NULL) {
  tag_assert(tag)

  authorized_lk <- c(
    "map_pressure", "map_light", "map_pressure_mse", "map_pressure_mask",
    "mask_water", "map_marginal"
  )

  # Automatic determination
  if (is.null(likelihood)) {
    pst <- authorized_lk %in% names(tag)

    # Priority 1: pressure x light
    if (pst[1] & pst[2]) {
      likelihood <- authorized_lk[c(1, 2)]
    } else {
      # Priority 2: in the order of authorized_lk
      i <- which(pst)[1]
      if (is.na(i)) {
        cli::cli_abort(c(
          x = "No map are are present in {.var tag}",
          i = "Make sure you've run {.fun geopressure_map} and/or {.fun geolight_map}"
        ))
      }
      likelihood <- authorized_lk[i]
    }
  } else {
    # Accept wrong name for pressure and light
    likelihood[likelihood == "pressure"] <- "map_pressure"
    likelihood[likelihood == "light"] <- "map_light"

    if (any(!(likelihood %in% authorized_lk))) {
      cli::cli_abort(c(
        "x" = "The likelihood map {.val {likelihood}} {? is/are} not authorized.",
        ">" = "{.var likelihood} should be one of {.val {authorized_lk}}"
      ))
    }
  }

  assertthat::assert_that(assertthat::has_name(tag, likelihood))

  return(likelihood)
}
