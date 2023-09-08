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
#' setwd(system.file("extdata", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = TRUE) |>
#'   tag_label(quiet = TRUE) |>
#'   twilight_create() |>
#'   twilight_label_read() |>
#'   tag_set_map(
#'     extent = c(-16, 23, 0, 50),
#'     known = data.frame(stap_id = 1, known_lon = 17.05, known_lat = 48.9)
#'   ) |>
#'   geopressure_map(quiet = TRUE) |>
#'   geolight_map(quiet = TRUE)
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

  cli::cli_h1("GeoPressureR `graph` object for {.field id}={.val {graph$param$id}}")

  cli::cli_h3("Stationary periods {.field stap}")
  cli::cli_text("{.val {nrow(graph$stap)}} stationary period{?s}")
  print(utils::head(graph$stap))
  if (nrow(graph$stap) > 6) {
    cli::cli_text("Run {.code graph$stap} to display full table")
  }

  cli::cli_h3("Geographical parameters ({.field scale} and {.field extent})")
  # nolint start
  geo <- map_expand(graph$param$extent, graph$param$scale)
  # nolint end
  cli::cli_bullets(c(
    "*" = "Extent W-E: {.val {graph$param$extent[1]}}\u00b0 to \\
    {.val {graph$param$extent[2]}}\u00b0",
    "*" = "Extent S-N: {.val {graph$param$extent[3]}}\u00b0 to \\
    {.val {graph$param$extent[4]}}\u00b0",
    "*" = "Dimension lat-lon: {.val {geo$dim[1]}} x {.val {geo$dim[2]}}",
    "*" = "Resolution lat-lon: {.val {1/graph$param$scale}}\u00b0"
  ))

  cli::cli_h3("Graph size")
  geo <- map_expand(graph$param$extent, graph$param$scale)
  cli::cli_bullets(c(
    "*" = "{.val {length(graph$equipment)}} equipement node{?s}",
    "*" = "{.val {length(graph$retrieval)}} retrieval node{?s}",
    "*" = "{prettyNum(length(unique(c(graph$equipment, graph$t))), big.mark=',')} nodes",
    "*" = "{prettyNum(length(graph$s), big.mark=',')} edges"
  ))

  cli::cli_h3("Movement model")
  if ("ws" %in% names(graph)) {
    cli::cli_bullets(c("v" = "Windspeed computed!"))
  } else {
    cli::cli_bullets(c("!" = "Windspeed not computed. Use {.fun graph_add_wind}"))
  }

  if ("movement" %in% names(graph$param)) {
    cli::cli_bullets(c("v" = "Movement model defined for {.field {graph$param$movement$type}}"))
  } else {
    cli::cli_bullets(c("x" = "No movement model defined. Use {.fun graph_set_movement}"))
  }

  return(invisible(graph))
}
