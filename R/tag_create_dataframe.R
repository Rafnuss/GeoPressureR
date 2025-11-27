# Create from data.frame
#' @noRd
tag_create_dataframe <- function(
  id,
  pressure_file = NULL,
  light_file = NULL,
  acceleration_file = NULL,
  temperature_external_file = NULL,
  temperature_internal_file = NULL,
  magnetic_file = NULL,
  quiet = FALSE
) {
  # Create tag
  tag <- structure(list(param = param_create(id = id)), class = "tag")

  # Read Pressure
  if (!is.null(pressure_file)) {
    assertthat::assert_that(is.data.frame(pressure_file))
    assertthat::assert_that(assertthat::has_name(
      pressure_file,
      c("date", "value")
    ))
    assertthat::assert_that(inherits(pressure_file$date, "POSIXct"))
    assertthat::assert_that(assertthat::are_equal(
      attr(pressure_file$date, "tzone"),
      "UTC"
    ))
    if (
      min(pressure_file$value, na.rm = TRUE) < 250 ||
        1100 < max(pressure_file$value, na.rm = TRUE)
    ) {
      cli::cli_warn(
        "Pressure observation should be between 250 hPa (~10000m) and 1100 hPa \\
    (sea level at 1013hPa). Check unit of pressure data.frame provided."
      )
    }
    tag$pressure <- pressure_file
    tag$param$tag_create$pressure_file <- "df"
  }

  # Read light
  if (!is.null(light_file)) {
    assertthat::assert_that(assertthat::has_name(
      light_file,
      c("date", "value")
    ))
    assertthat::assert_that(inherits(light_file$date, "POSIXct"))
    assertthat::assert_that(assertthat::are_equal(
      attr(light_file$date, "tzone"),
      "UTC"
    ))
    tag$light <- light_file
    tag$param$tag_create$light_file <- "df"
  }

  # Read acceleration
  if (!is.null(acceleration_file)) {
    assertthat::assert_that(assertthat::has_name(
      acceleration_file,
      c("date", "value")
    ))
    assertthat::assert_that(inherits(acceleration_file$date, "POSIXct"))
    assertthat::assert_that(assertthat::are_equal(
      attr(acceleration_file$date, "tzone"),
      "UTC"
    ))
    tag$acceleration <- acceleration_file
    tag$param$tag_create$acceleration_file <- "df"
  }

  # Read acceleration
  if (!is.null(temperature_external_file)) {
    assertthat::assert_that(assertthat::has_name(
      temperature_external_file,
      c("date", "value")
    ))
    assertthat::assert_that(inherits(temperature_external_file$date, "POSIXct"))
    assertthat::assert_that(assertthat::are_equal(
      attr(temperature_external_file$date, "tzone"),
      "UTC"
    ))
    tag$temperature <- temperature_external_file
    tag$param$tag_create$temperature_external_file <- "df"
  }

  # Read air temperature
  if (!is.null(temperature_internal_file)) {
    assertthat::assert_that(assertthat::has_name(
      temperature_internal_file,
      c("date", "value")
    ))
    assertthat::assert_that(inherits(temperature_internal_file$date, "POSIXct"))
    assertthat::assert_that(assertthat::are_equal(
      attr(temperature_internal_file$date, "tzone"),
      "UTC"
    ))
    tag$temperature_internal <- temperature_internal_file
    tag$param$tag_create$temperature_internal_file <- "df"
  }

  # Read magnetism
  if (!is.null(magnetic_file)) {
    assertthat::assert_that(assertthat::has_name(
      magnetic_file,
      c(
        "date",
        "acceleration_x",
        "acceleration_y",
        "acceleration_z",
        "magnetic_x",
        "magnetic_y",
        "magnetic_z"
      )
    ))
    assertthat::assert_that(inherits(magnetic_file$date, "POSIXct"))
    assertthat::assert_that(assertthat::are_equal(
      attr(magnetic_file$date, "tzone"),
      "UTC"
    ))
    tag$magnetic <- magnetic_file
    tag$param$tag_create$magnetic_file <- "df"
  }

  tag$param$tag_create$manufacturer <- "df"
  tag$param$tag_create$directory <- "(not used)"

  return(tag)
}
