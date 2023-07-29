#' Assert status of a `graph` object
#'
#' These functions return logical about the contents of a `graph` object.
#'
#' @param graph a GeoPressureR `graph` object
#' @param cond condition to assert `tag` for. One of "tag" (default), "label", "stap", "geostap",
#' "pressure_map" and "map_pressure_mismatch", "twilight"
#'
#' @return logical indicating the `tag` object has the relevant element
#' @export
graph_assert <- function(graph, cond = "graph"){

  status <- graph_status(graph)

  if (cond == "graph"){
    return(TRUE)
  } else if (cond == "movement"){
    msg <- c(
      "x" = "The `graph` object has not movement model.",
      ">" = "Use {.fun graph_add_movement} to define the movement model."
    )
  } else if (cond == "full"){
    msg <- c(
      "x" = "The `graph` object is empty. No nodes nor edges are left at the construction.",
      ">" = "Check the input and run {.fun graph_create} again."
    )
  } else {
    stop(glue::glue("Condition {.var {cond}} is unknown"))
  }

  if (cond %in% status)
    return(TRUE)

  cli::cli_abort(msg)
}

#' Return status of a `graph`
#'
#' These functions return a vector of the status of `graph`.
#'
#'
#' @param graph a `graph` object
#'
#' @return logical indicating the `tag` object has the relevant element
#' @noRd
graph_status <- function(graph){
  assertthat::assert_that(inherits(graph, "graph"))

  assertthat::assert_that(assertthat::has_name(graph, c("id", "s", "t", "gs", "obs", "sz", "stap",
                                                        "equipment", "retrieval", "extent", "scale",
                                                        "mask_water")))

  status <- c()

  if (assertthat::has_name(graph, "movement")){
    status <- append(status, "movement")
  }
  if (length(graph$s) > 0){
    status <- append(status, "full")
  }
  return(status)
}
