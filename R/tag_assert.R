#' Assert status of a `tag`
#'
#' These functions return logical about the contents of a `tag` object.
#'
#'
#' @param tag a `tag` object
#' @param condition condition to assert `tag` for. One of "tag" (default), "read", "pressure",
#' "light", "acceleration", "label", "stap", "setmap", "pressure_map" and "map_pressure_mismatch",
#' "twilight"
#' @param type Message type to display. One of "abort" (default), "warn" or "inform"
#'
#' @return logical indicating the `tag` object has the relevant element
#' @export
tag_assert <- function(tag, condition = "tag", type = "abort") {
  status <- tag_status(tag)

  if (condition == "tag") {
    msg <- c("x" = "tag is not a `tag` object.")
  } else if (condition == "read") {
    msg <- c(
      "x" = "The `tag` object has no data.",
      ">" = "Use {.fun tag_read} to add data."
    )
  } else if (condition == "pressure") {
    msg <- c(
      "x" = "The `tag` object does not have `pressure` data."
    )
  }else if (condition == "light") {
    msg <- c(
      "x" = "The `tag` object does not have `light` data."
    )
  }else if (condition == "acceleration") {
    msg <- c(
      "x" = "The `tag` object does not have `acceleration` data."
    )
  } else if (condition == "label") {
    msg <- c(
      "x" = "The `tag` object has not yet been labeled.",
      ">" = "Use {.fun tag_label} to define the stationary periods."
    )
  } else if (condition == "stap") {
    msg <- c(
      "x" = "The stationary period have not yet been computed for `tag`.",
      ">" = "Use {.fun tag_label} to define the stationary periods."
    )
  } else if (condition == "setmap") {
    msg <- c(
      "x" = "The parameters for the geographical and stationary period have not been yet been defined in `tag`.",
      ">" = "Use {.fun tag_setmap} to define them."
    )
  } else if (condition == "map_pressure") {
    msg <- c(
      "x" = "The pressure likelihood map has not yet been computed for `tag`.",
      ">" = "Use {.fun geopressure_map} to compute the maps."
    )
  } else if (condition == "map_pressure_mismatch") {
    msg <- c(
      "x" = "The pressure mean square error map has not yet been computed for `tag`.",
      ">" = "Use {.fun geopressure_map_mismatch} to compute the maps."
    )
  } else if (condition == "twilight") {
    msg <- c(
      "x" = "The twilight has not yet been computed for `tag`",
      ">" = "Use {.fun twilight_create} to compute the twilight"
    )
  } else if (condition == "map_light") {
    msg <- c(
      "x" = "The light likelihood map has not yet been computed for `tag`.",
      ">" = "Use {.fun geolight_map} to compute the maps."
    )
  } else {
    stop(glue::glue("condition {.var {condition}} is unknown"))
  }

  if (condition %in% status) {
    return(TRUE)
  }

  if (type == "inform"){
    cli::cli_inform(msg)
  }else if (type == "warn"){
    cli::cli_warn(msg)
  }else {
    cli::cli_abort(msg)
  }

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

  if (!inherits(tag, "tag")){
    return(c())
  }
  status <- c("tag")

  if (assertthat::has_name(tag, "pressure")) {
    status <- append(status, c("read", "pressure"))
  }
  if (assertthat::has_name(tag, "light")) {
    status <- append(status, "light")
  }
  if (assertthat::has_name(tag, "acceleration")) {
    status <- append(status, "acceleration")
  }
  if (assertthat::has_name(tag$pressure, "label")) {
    status <- append(status, "label")
  }
  if (assertthat::has_name(tag, "stap") &
    assertthat::has_name(tag$stap, "stap_id")) {
    status <- append(status, "stap")
  }
  if (assertthat::has_name(tag$param, c("extent", "scale")) &
    assertthat::has_name(tag$stap, c("known_lat", "known_lon", "include"))) {
    status <- append(status, "setmap")
  }
  if (assertthat::has_name(tag, "map_pressure")) {
    status <- append(status, "map_pressure")
  }
  if (assertthat::has_name(tag, c("map_pressure_mse", "map_pressure_mask"))) {
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

#' Status to factor
#'
#' These functions return a vector of the status of `tag`.
#'
#'
#' @param tag a `tag` object
#'
#' @return logical indicating the `tag` object has the relevant element
#' @noRd
status2factor <- function(status) {
  factor(status, levels = c("tag", ""))
}


# status = list(
#     name = "tag",
#     condition = \(tag) assertthat::has_name(tag, "map_light"),
#     msg = c(
#       "x" = "The light likelihood map has not yet been computed for `tag`.",
#       ">" = "Use {.fun geolight_map} to compute the maps."
#     )
#   )
# )
