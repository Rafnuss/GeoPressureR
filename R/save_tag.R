#' Save `tag`
#'
#' This function save a `tag` as a RDS file using the default file location
#
#' @param tag A GeoPressureR `tag` object
#' @param ... arguments passed from other methods
#'
#' @return `tag` is returned invisibly and unchanged
#' @family tag
#' @export
save_tag <- function(tag, file = glue::glue("data/interim/tag-{tag$param$id}.rds"), ...) {
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
  saveRDS(tag, file = file, ...)
  cli::cli_alert_success("{.var tag} saved successfully as {.file {file}}.")
}
