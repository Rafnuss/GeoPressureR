#' Assert the status of a `graph`
#'
#' This function check the condition of a `graph` object.
#'
#' @param graph a GeoPressureR `graph` object
#' @param condition condition to assert `graph` for. One of `"graph"` (default), `"movement"`,
#' or `"full"`.
#'
#' @return logical indicating whether the `graph` object fulfil the condition.
#' @export
graph_assert <- function(graph, condition = "graph") {
  status <- graph_status(graph)

  if (condition == "graph") {
    return(TRUE)
  } else if (condition == "movement") {
    msg <- c(
      "x" = "The `graph` object has not movement model.",
      ">" = "Use {.fun graph_set_movement} to define the movement model."
    )
  } else if (condition == "full") {
    msg <- c(
      "x" = "The `graph` object is empty. No nodes nor edges are left at the construction.",
      ">" = "Check the input and run {.fun graph_create} again."
    )
  } else {
    stop(glue::glue("condition {.var {condition}} is unknown"))
  }

  if (condition %in% status) {
    return(TRUE)
  }

  cli::cli_abort(msg)
}

#' @noRd
graph_status <- function(graph) {
  assertthat::assert_that(inherits(graph, "graph"))

  assertthat::assert_that(assertthat::has_name(graph, c(
    "s", "t", "gs", "obs", "sz", "stap",
    "equipment", "retrieval", "mask_water"
  )))

  status <- c()

  if (assertthat::has_name(graph$param, "graph_set_movement")) {
    status <- append(status, "movement")
  }
  if (length(graph$s) > 0) {
    status <- append(status, "full")
  }
  return(status)
}
