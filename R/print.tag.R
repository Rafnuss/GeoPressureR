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
#' setwd(system.file("extdata", package = "GeoPressureR"))
#'
#' tag <- tag_create("18LX", quiet = TRUE)
#'
#' print(tag)
#'
#' @family tag
#' @method print tag
#' @export
print.tag <- function(x, ...) {
  tag <- x

  status <- tag_status(tag)

  cli::cli_h1("GeoPressureR `tag` object for {tag$param$id}")
  cli::cli_text("{.strong Note}: All {.field green} texts are fields of `tag` (i.e., \\
                `tag${.field field}`).")

  # Param
  cli::cli_h3("Parameter {.field param}")
  cli::cli_text("Run {.code tag$param} to display full table")

  if (!("read" %in% status)) {
    cli::cli_bullets(c("x" = "Sensors data not yet read. Use {.fun tag_create}"))
  } else {
    cli::cli_h3("Sensors data")
    cli::cli_text("Manufacturer: {tag$param$manufacturer}")
    cli::cli_text("Date range: {tag$pressure$date[1]} to {tail(tag$pressure$date,1)}")
    cli::cli_bullets(c("*" = "{.field pressure}: {nrow(tag$pressure)} datapoints"))
    if ("acceleration" %in% names(tag)) {
      cli::cli_bullets(c("*" = "{.field acceleration}: {nrow(tag$acceleration)} datapoints"))
    }
    if ("light" %in% names(tag)) {
      cli::cli_bullets(c("*" = "{.field light}: {nrow(tag$light)} datapoints"))
    }
    if ("temperature" %in% names(tag)) {
      cli::cli_bullets(c("*" = "{.field temperature}: {nrow(tag$temperature)} datapoints"))
    }
    if ("twilight" %in% names(tag)) {
      cli::cli_bullets(c("*" = "{.field twilight}: {nrow(tag$twilight)} datapoints"))
    }

    # Stationary periods
    cli::cli_h3("Stationary periods {.field stap}")
    if (!("stap" %in% names(tag))) {
      cli::cli_bullets(c("x" = "No stationary periods defined yet. Use {.fun tag_label}"))
    } else {
      cli::cli_text("{.val {nrow(tag$stap)}} stationary periods")
      print(utils::head(tag$stap, n = 3))
      if (nrow(tag$stap) > 3) {
        cli::cli_text("...")
        cli::cli_text("Run {.code tag$stap} to see full stap table")
      }

      # Map
      cli::cli_h3("Map")
      if (!("setmap" %in% status)) {
        cli::cli_bullets(c("x" = "No geographical parameters defined yet. Use {.fun tag_set_map}"))
      } else {
        # nolint start
        geo <- map_expand(tag$param$extent, tag$param$scale)
        cli::cli_bullets(c(
          "*" = "Extent (W, E, S, N): {.val {tag$param$extent[1]}}\u00b0, \\
        {.val {tag$param$extent[2]}}\u00b0, {.val {tag$param$extent[3]}}\u00b0, \\
        {.val {tag$param$extent[4]}}\u00b0",
          "*" = "Dimensions (lat x lon): {.val {geo$dim[1]}} x {.val {geo$dim[2]}} (res. \\
          {.val {1/tag$param$scale}}\u00b0)"
        ))

        map_pressure_mismatch <- c("map_pressure_mse", "map_pressure_mask")
        map_pressure_mismatch <- map_pressure_mismatch[map_pressure_mismatch %in% names(tag)]
        if (length(map_pressure_mismatch) > 0) {
          cli::cli_bullets(c(
            "v" = "Pressure mismatch {.field {map_pressure_mismatch}} available."
          ))
        }
        # nolint end
        if ("map_pressure" %in% status) {
          cli::cli_bullets(c(
            "v" = "Pressure likelihood {.field map_pressure} computed!"
          ))
        } else {
          if ("map_pressure_mse" %in% names(tag)) {
            fun_map <- "geopressure_map_likelihood"
          } else {
            fun_map <- "geopressure_map"
          }
          cli::cli_bullets(c(
            "x" = "No pressure likelihood computed yet. Use {.fun {fun_map}}."
          ))
        }
        if ("map_light" %in% status) {
          cli::cli_bullets(c("v" = "Light likelihood {.field map_light} computed!"))
        }
      }
    }
  }
  return(invisible(tag))
}
