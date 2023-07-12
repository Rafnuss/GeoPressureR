#' Assert status of a `tag`
#'
#' These functions return logical about the contents of a `tag` object.
#'
#'
#' @param tag a `tag` object
#'
#' @return logical indicating the `tag` object has the relevant element
#' @export
tag_assert <- function(tag, cond = "tag"){

  assertthat::assert_that(is.tag(tag))

  if (cond == "tag"){
    return(TRUE)
  } else if (cond == "label"){
    res <- assertthat::has_name(tag$pressure, "label")
    msg <- c(
      "x" = "The `tag` object has not yet been labeled.",
      ">" = "Use {.fun tag_label} to define the stationary periods."
    )
  } else if (cond == "stap"){
    res <- assertthat::has_name(tag, "stap")
    msg <- c(
      "x" = "The stationary period have not yet been computed for `tag`.",
      ">" = "Use {.fun tag_label} to define the stationary periods."
    )
  } else if (cond == "geostap"){
    res <- assertthat::has_name(tag, "extent") &
      assertthat::has_name(tag, "scale") &
      assertthat::has_name(tag$stap, "known_lat") &
      assertthat::has_name(tag$stap, "known_lon") &
      assertthat::has_name(tag$stap, "include")
    msg <- c(
      "x" = "The parameters for the geographical and stationary period have been yet been defined in `tag`.",
      ">" = "Use {.fun tag_geostap} to define them."
    )
  } else if (cond == "pressure_map"){
    res <- assertthat::has_name(tag, "pressure_map")
    msg <- c(
      "x" = "The pressure likelihood map has not yet been computed for `tag`.",
      ">" = "Use {.fun geopressure_map} to compute the maps."
    )
  } else if (cond == "map_pressure_mismatch"){
    res <- assertthat::has_name(tag, "map_pressure_mse") & assertthat::has_name(tag, "map_pressure_mask")
    msg <- c(
      "x" = "The pressure mean square error map has not yet been computed for `tag`.",
      ">" = "Use {.fun geopressure_map_mismatch} to compute the maps."
    )
  } else if (cond == "twilight"){
    res <- assertthat::has_name(tag, "twilight")
    msg <- c(
      "x" = "The twilight has not yet been computed for `tag`",
      ">" = "Use {.fun twilight_create} to compute the twilight"
    )
  }else {
    stop(glue::glue("Condition {.var {cond}} is unknown"))
  }

  if (res)
    return(TRUE)

  cli::cli_abort(msg)
}
