#' Read British Antarctic Survey
#'
#' BASTrak (.lig) files
#' Each line represents a ten minute period. Errors are shown on additional lines.
#' <ok/suspect>,<DD/MM/YY hh:mm:ss>,<seconds reference>,<light>
#' where
#' - <ok/suspect> indicates whether the data is ok or suspect
#' - <DD/MM/YY hh:mm:ss> is the time stamp
#' - <seconds reference> is another way of representing the time; it is the number of
#' seconds elapsed since the reference chosen when the file was processed
#' - <light> is the maximum light value measured during the previous 10 minutes
#' @noRd
tag_create_bas <- function(
  id,
  directory = glue::glue("./data/raw-tag/{id}"),
  lig_file = NULL,
  act_file = NULL,
  quiet = FALSE
) {
  assertthat::assert_that(is.character(id))
  assertthat::assert_that(is.logical(quiet))

  # Create tag
  tag <- structure(list(param = param_create(id = id)), class = "tag")

  # Read Pressure
  if (is.null(lig_file)) {
    lig_file <- "*.lig"
  }
  lig_path <- tag_create_detect(lig_file, directory, quiet = quiet)
  if (is.null(lig_path)) {
    cli::cli_abort(c(
      "x" = "There are no file {.val {lig_file}}",
      "!" = "{.var lig_file} is required!"
    ))
  }

  # Read file
  data_raw <- utils::read.delim(lig_path, sep = ",", header = FALSE)
  tag$light <- data.frame(
    date = as.POSIXct(strptime(
      data_raw[, 2],
      tz = "UTC",
      format = "%d/%m/%y %H:%M:%S"
    )),
    value = data_raw[, 4]
  )

  # Add parameter information
  tag$param$tag_create$light_file <- lig_file
  tag$param$tag_create$manufacturer <- "bas"
  tag$param$tag_create$directory <- directory

  return(tag)
}
