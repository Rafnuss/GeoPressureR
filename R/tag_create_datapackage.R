# Read GeoLocator Data Package files
#' @noRd
tag_create_datapackage <- function(
  id,
  directory = glue::glue("./data/raw-tag/{id}"),
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
  if (is.null(pressure_file)) {
    pressure_path <- "pressure.csv"
  }
  pressure_path <- file.path(directory, "pressure.csv")
  if (file.exists(pressure_path)) {
    tag$pressure <- tag_create_csv(
      pressure_path,
      col_name = c("datetime", "value"),
      quiet = quiet
    )
  }

  # Read light
  if (is.null(light_file)) {
    light_file <- "light.csv"
  }
  light_path <- file.path(directory, light_file)
  if (file.exists(light_path)) {
    tag$light <- tag_create_csv(
      light_path,
      col_name = c("datetime", "value"),
      quiet = quiet
    )
  }

  # Read acceleration
  if (is.null(acceleration_file)) {
    acceleration_file <- "acceleration.csv"
  }
  acceleration_path <- file.path(directory, acceleration_file)
  if (file.exists(acceleration_path)) {
    tag$acceleration <- tag_create_csv(
      acceleration_path,
      col_name = c("datetime", "value"),
      quiet = quiet
    )
  }

  # Read external temperature
  if (is.null(temperature_external_file)) {
    temperature_external_file <- "temperature_external.csv"
  }
  temperature_external_path <- file.path(directory, temperature_external_file)
  if (file.exists(temperature_external_path)) {
    tag$temperature_external <- tag_create_csv(
      temperature_external_path,
      col_name = c("datetime", "value"),
      quiet = quiet
    )
  }

  # Read internal temperature
  if (is.null(temperature_internal_file)) {
    temperature_internal_file <- "temperature_internal.csv"
  }
  temperature_internal_path <- file.path(directory, temperature_internal_file)
  if (file.exists(temperature_internal_path)) {
    tag$temperature_internal <- tag_create_csv(
      temperature_internal_path,
      col_name = c("datetime", "value"),
      quiet = quiet
    )
  }

  # Read magnetism
  if (is.null(magnetic_file)) {
    magnetic_file <- "magnetic.csv"
  }
  magnetic_path <- file.path(directory, magnetic_file)
  if (file.exists(magnetic_path)) {
    tag$magnetic <- tag_create_csv(
      magnetic_path,
      col_name = c(
        "datetime",
        "magnetic_x",
        "magnetic_y",
        "magnetic_z",
        "acceleration_x",
        "acceleration_y",
        "acceleration_z"
      ),
      quiet = quiet
    )
  }

  # Add parameter information
  tag$param$tag_create$pressure_file <- pressure_path
  tag$param$tag_create$light_file <- light_path
  tag$param$tag_create$acceleration_file <- acceleration_path
  tag$param$tag_create$temperature_external_file <- temperature_external_path
  tag$param$tag_create$temperature_internal_file <- temperature_internal_path
  tag$param$tag_create$magnetic_file <- magnetic_path
  tag$param$tag_create$manufacturer <- "datapackage"
  tag$param$tag_create$directory <- directory

  return(tag)
}
