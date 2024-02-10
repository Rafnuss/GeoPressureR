#' Assert the status of a `tag`
#'
#' This function check the condition of a `tag` object.
#'
#' @param tag a GeoPressureR `tag` object.
#' @param condition condition to assert `tag` for. One of `"tag"` (default), `"pressure"`,
#' `"light"`, `"acceleration"`, `"label"`, `"stap"`, `"setmap"`, `"map_pressure"`, `"map_light"`
#' `"map_pressure_mse"` and `"twilight"`
#' @param type Message type to display. One of `"abort"` (default), `"warn"` or `"inform"`
#'
#' @return logical indicating the `tag` object has the relevant element
#'
#' @examples
#' setwd(system.file("extdata", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = TRUE) |> tag_label(quiet = TRUE)
#'
#' tag_assert(tag)
#'
#' tag_assert(tag, "stap")
#'
#' tag_assert(tag, "setmap", type = "warn")
#'
#' tag_assert(tag, "map_pressure", type = "inform")
#'
#' @export
tag_assert <- function(tag, condition = "tag", type = "abort") {
  status <- tag_status(tag)

  if (condition == "tag") {
    msg <- c("x" = "tag is not a {.var tag} object.")
  } else if (condition == "pressure") {
    msg <- c(
      "x" = "The {.var tag} object does not have {.field pressure} data."
    )
  } else if (condition == "light") {
    msg <- c(
      "x" = "The {.var tag} object does not have {.field light} data."
    )
  } else if (condition == "acceleration") {
    msg <- c(
      "x" = "The {.var tag} object does not have {.field acceleration} data."
    )
  } else if (condition == "label") {
    msg <- c(
      "x" = "The {.var tag} object has not yet been labelled.",
      ">" = "Use {.fun tag_label} to define the stationary periods."
    )
  } else if (condition == "stap") {
    msg <- c(
      "x" = "The stationary period have not yet been computed for {.var tag}.",
      ">" = "Use {.fun tag_label} to define the stationary periods."
    )
  } else if (condition == "setmap") {
    msg <- c(
      "x" = "The parameters for the geographical and stationary period have not been yet been \\
      defined in {.var tag}.",
      ">" = "Use {.fun tag_set_map} to define them."
    )
  } else if (condition == "map_pressure") {
    msg <- c(
      "x" = "The pressure likelihood map has not yet been computed for {.var tag}.",
      ">" = "Use {.fun geopressure_map} to compute the maps."
    )
  } else if (condition == "map_pressure_mse") {
    msg <- c(
      "x" = "The pressure mean square error map has not yet been computed for {.var tag}.",
      ">" = "Use {.fun geopressure_map_mismatch} to compute the maps."
    )
  } else if (condition == "twilight") {
    msg <- c(
      "x" = "The twilight has not yet been computed for {.var tag}",
      ">" = "Use {.fun twilight_create} to compute the twilight"
    )
  } else if (condition == "map_light") {
    msg <- c(
      "x" = "The light likelihood map has not yet been computed for {.var tag}.",
      ">" = "Use {.fun geolight_map} to compute the maps."
    )
  } else {
    stop(glue::glue("condition {.var condition} is unknown"))
  }

  if (condition %in% status) {
    return(TRUE)
  }

  if (type == "inform") {
    cli::cli_inform(msg)
  } else if (type == "warn") {
    cli::cli_warn(msg)
  } else {
    cli::cli_abort(msg)
  }
}

#' @noRd
tag_status <- function(tag) {
  if (!inherits(tag, "tag")) {
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
  if (assertthat::has_name(tag, "stap") &&
    assertthat::has_name(tag$stap, "stap_id")) {
    status <- append(status, "stap")
  }
  if (assertthat::has_name(tag$param, c("extent", "scale")) &&
    assertthat::has_name(tag$stap, c("known_lat", "known_lon", "include"))) {
    status <- append(status, "setmap")
  }
  if (assertthat::has_name(tag, "map_pressure")) {
    status <- append(status, "map_pressure")
  }
  if (assertthat::has_name(tag, "map_pressure_mse")) {
    status <- append(status, "map_pressure_mse")
  }
  if (assertthat::has_name(tag, "twilight")) {
    status <- append(status, "twilight")
  }
  if (assertthat::has_name(tag, "map_light")) {
    status <- append(status, "map_light")
  }

  return(status)
}
