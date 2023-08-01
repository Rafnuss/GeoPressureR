#' Read `tag`
#'
#' This function read a `tag` from a RDS file using the default file location
#
#' @param id A GeoPressureR `tag` object
#' @param ... arguments passed from other methods
#'
#' @return `tag` is returned
#' @family tag
#' @export
read_tag <- function(id, file = glue::glue("data/interim/tag-{id}.rds"), ...) {
  if (!file.exists(file)) {
    cli::cli_abort("The file {.file {file}} does not exists.")
  }

  tag <- readRDS(file = file, ...)
  cli::cli_alert_success("{.var tag} was read successfully from {.file {file}}.")

  return(tag)
}
