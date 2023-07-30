#' Assert status of a `tag`
#'
#' These functions return logical about the contents of a `tag` object.
#'
#'
#' @param tag a `tag` object
#' @param cond condition to assert `tag` for. One of "tag" (default), "label", "stap", "geostap",
#' "pressure_map" and "map_pressure_mismatch", "twilight"
#'
#' @return logical indicating the `tag` object has the relevant element
#' @export
tag_assert <- function(tag, cond = "tag") {
  status <- tag_status(tag)

  if (cond == "tag") {
    return(TRUE)
  } else if (cond == "label") {
    msg <- c(
      "x" = "The `tag` object has not yet been labeled.",
      ">" = "Use {.fun tag_label} to define the stationary periods."
    )
  } else if (cond == "stap") {
    msg <- c(
      "x" = "The stationary period have not yet been computed for `tag`.",
      ">" = "Use {.fun tag_label} to define the stationary periods."
    )
  } else if (cond == "geostap") {
    msg <- c(
      "x" = "The parameters for the geographical and stationary period have not been yet been defined in `tag`.",
      ">" = "Use {.fun tag_geostap} to define them."
    )
  } else if (cond == "map_pressure") {
    msg <- c(
      "x" = "The pressure likelihood map has not yet been computed for `tag`.",
      ">" = "Use {.fun geopressure_map} to compute the maps."
    )
  } else if (cond == "map_pressure_mismatch") {
    msg <- c(
      "x" = "The pressure mean square error map has not yet been computed for `tag`.",
      ">" = "Use {.fun geopressure_map_mismatch} to compute the maps."
    )
  } else if (cond == "twilight") {
    msg <- c(
      "x" = "The twilight has not yet been computed for `tag`",
      ">" = "Use {.fun twilight_create} to compute the twilight"
    )
  } else if (cond == "map_light") {
    msg <- c(
      "x" = "The light likelihood map has not yet been computed for `tag`.",
      ">" = "Use {.fun geolight_map} to compute the maps."
    )
  } else {
    stop(glue::glue("Condition {.var {cond}} is unknown"))
  }

  if (cond %in% status) {
    return(TRUE)
  }

  cli::cli_abort(msg)
}

#' Return status of a `tag`
#'
#' These functions return a vector of the status of `tag`.
#'
#'
#' @param tag a `tag` object
#'
#' @return logical indicating the `tag` object has the relevant element
#' @noRd
tag_status <- function(tag) {
  assertthat::assert_that(inherits(tag, "tag"))

  status <- c()
  if (assertthat::has_name(tag$pressure, "label")) {
    status <- append(status, "label")
  }
  if (assertthat::has_name(tag, "stap") &
    assertthat::has_name(tag$stap, "stap_id")) {
    status <- append(status, "stap")
  }
  if (assertthat::has_name(tag, "extent") &
    assertthat::has_name(tag, "scale") &
    assertthat::has_name(tag$stap, "known_lat") &
    assertthat::has_name(tag$stap, "known_lon") &
    assertthat::has_name(tag$stap, "include")) {
    status <- append(status, "geostap")
  }
  if (assertthat::has_name(tag, "map_pressure")) {
    status <- append(status, "map_pressure")
  }
  if (assertthat::has_name(tag, "map_pressure_mse") &
    assertthat::has_name(tag, "map_pressure_mask")) {
    status <- append(status, "map_pressure_mismatch")
  }
  if (assertthat::has_name(tag, "twilight")) {
    status <- append(status, "twilight")
  }
  if (assertthat::has_name(tag, "map_light")) {
    status <- append(status, "map_light")
  }

  return(status)
}
