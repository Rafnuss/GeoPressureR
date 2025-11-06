#' Print a `graph` object
#'
#' This function displays the information of a `graph` object.
#
#' @param x a GeoPressureR `graph` object.
#' @param ... arguments passed to other methods
#'
#' @return `graph` is returned invisibly and unchanged
#'
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |>
#'     tag_label(quiet = TRUE) |>
#'     twilight_create() |>
#'     twilight_label_read() |>
#'     tag_set_map(
#'       extent = c(-16, 23, 0, 50),
#'       known = data.frame(stap_id = 1, known_lon = 17.05, known_lat = 48.9)
#'     ) |>
#'     geopressure_map(quiet = TRUE) |>
#'     geolight_map(quiet = TRUE)
#' })
#'
#' graph <- graph_create(tag, quiet = TRUE)
#'
#' print(graph)
#'
#' @family graph
#' @method print graph
#' @export
print.graph <- function(x, ...) {
  graph <- x
  cli::cli_div(theme = list(".hint" = list(color = "grey60")))
  cli::cli_h1("GeoPressureR `graph` object for {graph$param$id}")

  cli::cli_text(
    "{.hint {.strong Note}: All {.field green} texts are fields of `graph` (i.e., \\
                `graph${.field field}`).}"
  )

  # Param
  cli::cli_h3("Parameters {.field param}")
  cli::cli_text("{.hint Run {.code graph$param} to display all parameters}")

  cli::cli_h3("Stationary periods {.field stap}")
  cli_print_tbl(graph$stap)

  cli::cli_h3("Map")
  # nolint start
  geo <- map_expand(
    graph$param$tag_set_map$extent,
    graph$param$tag_set_map$scale
  )
  cli::cli_bullets(c(
    "*" = "Extent (W, E, S, N): {.val {graph$param$tag_set_map$extent[1]}}\u00b0,
        {.val {graph$param$tag_set_map$extent[2]}}\u00b0,
        {.val {graph$param$tag_set_map$extent[3]}}\u00b0,
        {.val {graph$param$tag_set_map$extent[4]}}\u00b0",
    "*" = "Dimensions (lat x lon): {.val {geo$dim[1]}} x {.val {geo$dim[2]}} (res.
          {.val {1/graph$param$tag_set_map$scale}}\u00b0)"
  ))
  # nolint end

  cli::cli_h3("Graph size")
  cli::cli_bullets(c(
    "*" = "{.val {length(graph$equipment)}} {.field equipement} node{?s}",
    "*" = "{.val {length(graph$retrieval)}} {.field retrieval} node{?s}",
    "*" = "{prettyNum(length(unique(c(graph$equipment, graph$t))), big.mark=',')} nodes",
    "*" = "{prettyNum(length(graph$s), big.mark=',')} edges"
  ))

  cli::cli_h3("Movement model")
  if ("ws" %in% names(graph)) {
    cli::cli_bullets(c("v" = "Windspeed {.field ws} computed!"))
  } else {
    cli::cli_bullets(c(
      "!" = "Windspeed not computed. Use {.fun graph_add_wind}"
    ))
  }

  if ("movement" %in% names(graph$param)) {
    cli::cli_bullets(c(
      "v" = "Movement model defined for
                      {.field {graph$param$graph_set_movement$type}}"
    ))
  } else {
    cli::cli_bullets(c(
      "x" = "No movement model defined. Use {.fun graph_set_movement}"
    ))
  }
  cli::cli_end()
  invisible(graph)
}
