#' Read GeoPressureR object
#'
#' This function read a GeoPressureR object from a file
#
#' @param file File to be read
#' @param id GeoPressureR bird identification
#' @param type One of `"tag"`, `"graph"`, `"pressure_path"`, `"path"`
#'
#' @return `x` is returned
#' @export
read_geopressurer <- function(file, type, ...) {
  if (!file.exists(file)) {
    cli::cli_abort("The file {.file {file}} does not exists.")
  }

  x <- readRDS(file = file, ...)

  cli::cli_alert_success("{.var {type}} was read successfully from {.file {file}}.")

  return(x)
}


#' @describeIn read_geopressurer Save a GeoPressureR `tag` object
#' @export
read_tag <- function(id, file = glue::glue("data/interim/tag-{id}.rds"), ...) {
  return(read_geopressurer(file, type = "tag"))
}

#' @describeIn read_geopressurer Save a GeoPressureR `graph` object
#' @export
read_graph <- function(id, file = glue::glue("data/interim/graph-{id}.rds"), ...) {
  return(read_geopressurer(file, type = "graph"))
}

#' @describeIn read_geopressurer Save a GeoPressureR `pressurepath` object
#' @export
read_pressurepath <- function(id, file = glue::glue("data/interim/pressurepath-{id}.rds"), ...) {
  return(read_geopressurer(file, type = "pressurepath"))
}
