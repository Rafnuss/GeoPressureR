#' Print a `tag` object
#'
#' This function displays the information of a `tag` object.
#
#' @param x a GeoPressureR `tag` object
#' @param ... further arguments passed to or from other methods.
#'
#' @return `tag` is returned invisibly and unchanged
#'
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = TRUE)
#' print(tag)
#'
#' @family tag
#' @method print tag
#' @export
print.tag <- function(x, ...) {
  tag <- x

  out <- cli::cli_fmt({
    cli::cli_h1("GeoPressureR `tag` object for {.field id}={.val {tag$param$id}}")

    status <- tag_status(tag)

    if (!("read" %in% status)) {
      out <- c(out, cli::cli_fmt({
        cli::cli_bullets(c("x" = "Sensors data not yet read. Use {.fun tag_create}"))
      }))
    } else {
      cli::cli_text("Date range: {tag$pressure$date[1]} to {tail(tag$pressure$date,1)}")
      cli::cli_h3("Sensors data")
      cli::cli_bullets("{.field pressure}: {nrow(tag$pressure)} datapoints")
      if ("acceleration" %in% status) {
        cli::cli_bullets("{.field acceleration}: {nrow(tag$acceleration)} datapoints")
      }
      if ("light" %in% status) {
        cli::cli_bullets("{.field light}: {nrow(tag$light)} datapoints")
      }

      # Stationary periods
      cli::cli_h3("Stationary periods {.field stap}")
      if (!("stap" %in% status)) {
        cli::cli_bullets(c("x" = "No stationary periods defined yet. Use {.fun tag_label}"))
      } else {
        cli::cli_text("{.val {nrow(tag$stap)}} stationary periods")
        cli::cli_text("Run {.code tag$stap} to see stap table")

        # Geographical
        cli::cli_h3("Geographical parameters ({.field scale} and {.field extent})")
        if (!("setmap" %in% status)) {
          cli::cli_bullets(c("x" = "No geographical parameters defined yet. Use {.fun tag_set_map}"))
        } else {
          geo <- map_expand(tag$param$extent, tag$param$scale)
          cli::cli_bullets(c(
            "*" = "Extent W-E: {.val {tag$param$extent[1]}}\u00b0 to {.val {tag$param$extent[2]}}\u00b0",
            "*" = "Extent S-N: {.val {tag$param$extent[3]}}\u00b0 to {.val {tag$param$extent[4]}}\u00b0",
            "*" = "Dimension lat-lon: {.val {geo$dim[1]}} x {.val {geo$dim[2]}}\u00b0",
            "*" = "Resolution lat-lon: {.val {1/tag$param$scale}}\u00b0"
          ))

          # Map
          cli::cli_h3("Map")
          if ("map_pressure" %in% status) {
            cli::cli_bullets(c("v" = "Pressure likelihood map {.field map_pressure} computed!"))
            if ("map_pressure_mse" %in% status) {
              cli::cli_bullets(c("i" = "Pressure mismatched maps {.field map_pressure_mse} and \\
                              {.field map_pressure_mask} are also available."))
            }
          } else {
            if ("map_pressure_mse" %in% status) {
              cli::cli_bullets(c("!" = "Pressure mismatched maps {.field map_pressure_mse} computed and \\
                              {.field map_pressure_mask}, but not the likelihood map. Use \\
                              {.fun geopressure_map_likelihood}."))
            } else {
              cli::cli_bullets(c("x" = "No pressure likelihood computed. Use {.fun geopressure_map}."))
            }
          }
          if ("map_light" %in% status) {
            cli::cli_bullets(c("v" = "Light likelihood {.field map_light} computed!"))
          }

          # Param
          cli::cli_h3("Parameter {.field param}")
          cli::cli_text("Run {.code tag$param} to display full table")
        }
      }
    }
  })

  cat(out, sep = "\n")
  return(invisible(tag))
}
