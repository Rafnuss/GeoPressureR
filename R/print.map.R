#' Print a `map` object
#'
#' This function displays the information of a `map` object.
#
#' @param x a GeoPressureR `map` object
#' @param ... arguments passed to other methods
#'
#' @return `map` is returned invisibly and unchanged
#'
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = TRUE) |>
#'   tag_label(quiet = TRUE) |>
#'   tag_label(quiet = TRUE) |>
#'   tag_set_map(
#'     extent = c(-16, 23, 0, 50),
#'     scale = 4
#'   ) |>
#'   geopressure_map(quiet = TRUE)
#'
#' print(tag$map_pressure)
#'
#' @family map
#' @method print map
#' @export
print.map <- function(x, ...) {
  map <- x

  cli::cli_h1("GeoPressureR `map` object  of {.field {map$type}} for {.field id}={.val {map$id}}")

  cli::cli_text("Extent W-E: {.val {map$extent[1]}}\u00b0 to {.val {map$extent[2]}}\u00b0")
  cli::cli_text("Extent S-N: {.val {map$extent[3]}}\u00b0 to {.val {map$extent[4]}}\u00b0")
  cli::cli_text("Dimension lat-lon: {.val {dim(map)[1]}} x {.val {dim(map)[2]}}\u00b0")
  cli::cli_text("Resolution lat-lon: {.val {1/map$scale}}\u00b0")

  cli::cli_h3("Stationary periods {.field stap} (n={.val {nrow(map$stap)}})")
  print(utils::head(map$stap))
  if (nrow(map$stap) > 6) {
    cli::cli_text("Run {.code map$stap} to display full table")
  }
}
