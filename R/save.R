#' Save GeoPressure object
#'
#' This function save a GeoPressureR object or data.frame from different type (`tag`, `graph`,
#' `pressure_path`, `path`) as a RDS file using the default file location for intermediate data
#' (i.e., `"data/interim/"`)
#
#' @param x A GeoPressureR object or data.frame
#' @param file The file to be save
#' @param type One of `"tag"`, `"graph"`, `"pressure_path"`, `"path"`
#'
#' @return `x` is returned invisibly and unchanged
#' @export
save_geopressurer <- function(x, file, type, overwrite = FALSE, ...) {

  # Check file size
  file_size <- format(object.size(x), units = "MB")
  if( file_size > 100*1024*1024) {
    cli::cli_alert_warning("The estimated size for this file will be {.val {file_size}} MB.")
    res <- utils::askYesNo("Do you still want to create it?")
    if (!res) {
      return(FALSE)
    }
  }

  # Check directory
  dir_file <- dirname(file)
  if (!dir.exists(dir_file)) {
    cli::cli_alert_warning("The directory {.file {file.path(getwd(), dir_file)}} does not exist.")
    res <- utils::askYesNo("Do you want to create it?")
    if (res) {
      dir.create(dir_file)
    } else {
      return(FALSE)
    }
  }

  # Check if file exist
  if (file.exists(file) & !overwrite) {
    cli::cli_alert_warning("The file {.file {file}} already exist. (use {.code overwrite = TRUE} to avoid this message)")
    res <- utils::askYesNo("Do you want to overwrite it?")
    if (!res) {
      return(FALSE)
    }
  }

  # Save
  saveRDS(x, file = file, ...)

  cli::cli_alert_success("{.var {type}} saved successfully as {.file {file}} ({round(file.info(file)$size/1024/1024,1)} MB).")

  return(invisible(x))
}


#' @describeIn save_geopressurer Save a GeoPressureR `tag` object.
#' @export
save_tag <- function(x, file = glue::glue("data/interim/tag-{x$param$id}.rds"), ...) {
  save_geopressurer(x, file = file, type = "tag", ...)
  return(invisible(x))
}

#' @describeIn save_geopressurer Save a GeoPressureR `graph` object.
#' @export
save_graph <- function(x, file = glue::glue("data/interim/graph-{x$param$id}.rds"), ...) {
  save_geopressurer(x, file = file, type = "graph", ...)
  return(invisible(x))
}

#' @describeIn save_geopressurer Save a GeoPressureR `pressurepath` data.frame
#' @export
save_pressurepath <- function(x, file = glue::glue("data/interim/pressurepath-{attr(x, 'id')}.rds"), ...) {
  save_geopressurer(x, file = file, type = "pressurepath", ...)
  return(invisible(x))
}
