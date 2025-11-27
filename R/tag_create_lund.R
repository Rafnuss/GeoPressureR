# Read Lund tag files
#' @noRd
tag_create_lund <- function(
  id,
  directory = glue::glue("./data/raw-tag/{id}"),
  pressure_file = NULL,
  acceleration_light_file = NULL,
  quiet = FALSE
) {
  assertthat::assert_that(is.character(id))
  assertthat::assert_that(is.logical(quiet))

  # Create tag
  tag <- structure(list(param = param_create(id = id)), class = "tag")

  # Read Pressure
  if (is.null(pressure_file)) {
    pressure_file <- "_press.xlsx"
  }
  pressure_path <- tag_create_detect(pressure_file, directory, quiet = quiet)
  if (is.null(pressure_path)) {
    cli::cli_abort(c(
      "x" = "There are no file {.val {pressure_path}}",
      "!" = "{.var pressure_path} is required"
    ))
  }
  xls <- readxl::read_excel(pressure_path, .name_repair = "unique_quiet")
  tag$pressure <- data.frame(
    date = as.POSIXct(xls$`Date & time`, tz = "UTC"),
    value = xls$`Pressure [hPa]`
  )
  # Remove empty value
  tag$pressure <- tag$pressure[!is.na(tag$pressure$value), ]
  if (!quiet) {
    cli::cli_bullets(c("v" = "Read {.file {pressure_path}}"))
  }

  # Read light
  if (is.null(acceleration_light_file)) {
    acc_light_path <- tag_create_detect("_acc.xlsx", directory, quiet = TRUE)
  } else {
    acc_light_path <- tag_create_detect(
      acceleration_light_file,
      directory,
      quiet = quiet
    )
  }
  if (!is.null(acc_light_path)) {
    xls <- readxl::read_excel(
      acc_light_path,
      sheet = "Light",
      skip = 1,
      .name_repair = "unique_quiet"
    )
    tag$light <- data.frame(
      date = as.POSIXct(xls$`Date`, tz = "UTC"),
      value = xls$`Light`
    )

    # Read acceleration
    xls <- readxl::read_excel(acc_light_path, .name_repair = "unique_quiet")
    tag$acceleration <- data.frame(
      date = as.POSIXct(xls$`Activity date & time`, tz = "UTC"),
      value = xls$`Act score`
    )

    if (!quiet) {
      cli::cli_bullets(c("v" = "Read {.file {acc_light_path}}"))
    }
  }

  # Add parameter information
  tag$param$tag_create$pressure_file <- pressure_path
  tag$param$tag_create$light_file <- acc_light_path
  tag$param$tag_create$acceleration_file <- acc_light_path
  tag$param$tag_create$manufacturer <- "lund"
  tag$param$tag_create$directory <- directory

  return(tag)
}
