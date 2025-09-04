#' Load an interim RData object
#'
#' @description
#' Loads an object from the `./data/interim/` directory created during the GeoPressure analysis.
#' This is a convenience function to quickly restore saved objects (e.g., `tag`, `graph`,
#' `path_most_likely`) for a given tag `id`.
#'
#' @param id A character string of length 1 corresponding to the tag identifier used in the
#' GeoPressureTemplate (e.g., `"18LX"`). The function will look for a file named
#' `./data/interim/{id}.RData`.
#' @inheritParams base::load
#'
#' @return Invisibly returns the names of the objects loaded (as in [base::load()]).
#'
#' @export
load_interim <- function(id, envir = parent.frame(), verbose = FALSE) {
  assertthat::assert_that(is.character(id), length(id) == 1)
  file <- glue::glue("./data/interim/{id}.RData")

  if (!file.exists(file)) {
    cli::cli_abort(c(
      "x" = "File {.file {file}} does not exist.",
      "i" = "Ensure you have run the previous steps of the workflow or check the {id} spelling."
    ))
  }

  base::load(file, envir = envir, verbose = verbose)
}
