#' Create a `tag` object
#'
#' @description
#' Create a GeoPressureR `tag` object from the data collected by a tracking device. The function
#' can read data formatted according to three manufacturers SOI, Migratetech or Lund CAnMove, as
#' well as according to the GeoLocator Data package standard and also accept manual data.frame as
#' input. Pressure data is required for the GeoPressureR workflow but can be allowed to be missing
#' with `assert_pressure = FALSE`.
#'
#' @details
#' The current implementation can read files from the following three sources:
#' - [GeoLocator Data Package (`gldp`)](https://raphaelnussbaumer.com/GeoLocator-DP/)
#'    - `pressure_file = "pressure.csv"`(optional)
#'    - `light_file = "light.csv"` (optional)
#'    - `acceleration_file = "acceleration.csv"` (optional)
#'    - `temperature_external_file = "temperature.csv"` (optional)
#'    - `temperature_external_file = "airtemperature.csv"` (optional)
#'    - `magnetic_file = "magnetic.csv"` (optional)
#' - [Swiss Ornithological Institute (`soi`)](https://bit.ly/3QI6tkk)
#'    - `pressure_file = "*.pressure"`
#'    - `light_file = "*.glf"` (optional)
#'    - `acceleration_file = "*.acceleration"` (optional)
#'    - `temperature = "*.temperature"` (optional)
#'    - `airtemperature = "*.airtemperature"` (optional)
#'    - `magnetic = "*.magnetic"` (optional)
#' - [Migrate Technology (`migratetech`)](http://www.migratetech.co.uk/):
#'    - `pressure_file = "*.deg"`
#'    - `light_file = "*.lux"` (optional)
#'    - `acceleration_file = "*.deg"` (optional)
#' - British Antarctic Survey (`bas`), aquired by Biotrack Ltd in 2011, [renamed Lotek in 2019
#' ](https://www.lotek.com/about-us/history/). Only works for light data (`assert_pressure = FALSE`)
#'    - `light_file = "*.lig"`
#' - [Lund CAnMove (`lund`)](https://bit.ly/3P6quyi)
#'    - `pressure_file = "*_press.xlsx"`
#'    - `light_file = "*_acc.xlsx"` (optional)
#'    - `acceleration_file = "*_acc.xlsx"` (optional)
#' - [BitTag/PresTag (`prestag`)](https://geoffreymbrown.github.io/ultralight-tags/)
#'    - `pressure_file = "*.txt"`
#'
#' You can also enter the data manually (`manufacturer = "dataframe"`) by providing the data.frame:
#'   - `pressure_file`: data.frame with columns `date` and `value`.
#'   - `light_file`: (optional) data.frame with columns `date` and `value`.
#'   - `acceleration_file`: (optional) data.frame with columns `date` and `value`.
#'   - `temperature_external_file`: (optional) data.frame with columns `date` and `value`.
#'   - `temperature_internal_file`: (optional) data.frame with columns `date` and `value`.
#'   - `magnetic_file`: (optional) data.frame with columns `date`, `magnetic_x`, `magnetic_y`,
#'    `magnetic_z`, `acceleration_x`, `acceleration_y` and `acceleration_z`.
#'
#' You can still create a `tag` without pressure data using `assert_pressure = TRUE`. This `tag`
#' won't be able to run the traditional GeoPressureR workflow, but you can still do some analysis.
#'
#' By default `manufacturer = NULL`, the manufacturer is determined automatically from the content
#' of the `directory`. You can also specify manually the file with a full pathname or the file
#' extension using a regex expression (e.g., `"*.pressure"` matches any file ending with
#' `pressure`).
#'
#' Please create [an issue on Github](https://github.com/Rafnuss/GeoPressureR/issues/new) if you
#' have data in a format that is not yet supported.
#'
#' This function can be used to crop the data at specific date, for instance to remove pre-equipment
#' or post-retrieval data.
#'
#' @param id unique identifier of a tag.
#' @param manufacturer One of `NULL`, `"soi"`, `"migratetech"`, `"bas"`, `"lund"`, `"prestag"`,
#' `"datapackage"` or `"dataframe"`.
#' @param directory path of the directory where the tag files can be read.
#' @param pressure_file name of the file with pressure data. Full pathname  or finishing
#' with extensions (e.g., `"*.pressure"`, `"*.deg"` or `"*_press.xlsx"`).
#' @param light_file name of the file with light data. Full pathname  or finishing
#' with extensions (e.g., `"*.glf"`, `"*.lux"` or `"*_acc.xlsx"`).
#' @param acceleration_file name of the file with acceleration data. Full pathname  or finishing
#' with extensions (e.g., `"*.acceleration"`, `"*.deg"` or `"*_acc.xlsx"`).
#' @param temperature_external_file name of the file with temperature data. Full pathname  or
#' finishing with extensions (e.g., `"*.temperature"`, `"*.airtemperature"` or `"*.deg"`). External
#' or air temperature is generally for temperature sensor on directed outward from the bird.
#' @param temperature_internal_file name of the file with temperature data . Full pathname  or
#' finishing with extensions (e.g., `"*.bodytemperature"`). Internal or body temperature is
#' generally for temperature sensor on directed inward (between bird and tag).
#' @param magnetic_file name of the file with magnetic/accelerometer data. Full pathname  or
#' finishing with extensions (e.g., `"*.magnetic"`).
#' @param crop_start remove all data before this date (POSIXct or character in UTC).
#' @param crop_end remove all data after this date (POSIXct or character in UTC).
#' @param quiet logical to hide messages about the progress.
#' @param assert_pressure logical to check that the return tag has pressure data.
#'
#' @return a GeoPressureR `tag` object containing
#' - `param` parameter object (see [param_create()])
#' - `pressure` data.frame with columns: `date` and `value`
#' - `light` (optional) same structure as pressure
#' - `temperature_external` (optional) same structure as pressure
#' - `temperature_internal` (optional) same structure as pressure
#' - `acceleration` (optional) data.frame with columns: `date`, `value`, `act` and `pit`.
#'    - `value` is the activity computed as the sum of the difference in acceleration on the z-axis
#'    (i.e. jiggle). In the SOI sensor, it is summarised from 32 measurements at 10Hz
#'    - `pitch` is the relative position of the bird’s body relative to the z axis. In the SOI
#'    sensor, it is an average over 32 measurements at 10Hz.
#' - `magnetic` (optional) data.frame with columns: `date`, `magnetic_x`, `magnetic_y`, `magnetic_z`
#'    , `acceleration_x`, `acceleration_y` and `acceleration_z`
#'
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   # Read all sensor file
#'   tag <- tag_create("18LX")
#'
#'   print(tag)
#'
#'   # Read only pressure and crop date
#'   tag <- tag_create("18LX",
#'     light_file = NULL,
#'     acceleration_file = NULL,
#'     crop_start = "2017-08-01",
#'     crop_end = "2017-08-05"
#'   )
#'
#'   print(tag)
#'
#'   # You can also specify the exact file in case multiple files with the
#'   # same extension exist in your directory (migratetech data)
#'   tag <- tag_create("CB621",
#'     pressure_file = "CB621_BAR.deg",
#'     light_file = "CB621.lux",
#'     acceleration_file = NULL
#'   )
#'
#'   print(tag)
#'
#'   # You can specify the data manually with
#'   pressure <- data.frame(
#'     date = as.POSIXct(c(
#'       "2017-06-20 00:00:00 UTC", "2017-06-20 01:00:00 UTC",
#'       "2017-06-20 02:00:00 UTC", "2017-06-20 03:00:00 UTC"
#'     ), tz = "UTC"),
#'     value = c(1000, 1000, 1000, 1000)
#'   )
#'   tag_create(id = "xxx", pressure_file = pressure)
#' })
#'
#' @family tag
#' @seealso [GeoPressureManual](https://bit.ly/4462jpr)
#' @export
tag_create <- function(id,
                       manufacturer = NULL,
                       crop_start = NULL,
                       crop_end = NULL,
                       directory = glue::glue("./data/raw-tag/{id}"),
                       pressure_file = NULL,
                       light_file = NULL,
                       acceleration_file = NULL,
                       temperature_external_file = NULL,
                       temperature_internal_file = NULL,
                       magnetic_file = NULL,
                       assert_pressure = TRUE,
                       quiet = FALSE) {
  assertthat::assert_that(is.character(id))
  assertthat::assert_that(is.logical(quiet))

  if (is.null(manufacturer)) {
    if (is.data.frame(pressure_file)) {
      manufacturer <- "dataframe"
    } else {
      assertthat::assert_that(assertthat::is.dir(directory))
      if (any(grepl("pressure\\.csv$|light\\.csv$", list.files(directory)))) {
        manufacturer <- "datapackage"
      } else if (any(grepl("\\.(pressure|glf)$", list.files(directory)))) {
        manufacturer <- "soi"
      } else if (any(grepl("\\.deg$", list.files(directory)))) {
        manufacturer <- "migratetech"
      } else if (any(grepl("\\.lig$", list.files(directory)))) {
        manufacturer <- "bas"
      } else if (any(grepl("_press\\.xlsx$", list.files(directory)))) {
        manufacturer <- "lund"
      } else {
        cli::cli_abort(c(
          "x" = "We were not able to determine the {.var manufacturer} of tag from the directory
        {.file {directory}}",
          ">" = "Check that this directory contains the file with pressure data (i.e., with
        extension {.val .csv}, {.val .pressure}, {.val .glf}, {.val .deg} or {.val _press.xlsx})"
        ))
      }
    }
  }
  assertthat::assert_that(is.character(manufacturer))
  manufacturer_possible <- c(
    "datapackage", "soi", "migratetech", "bas", "prestag", "lund", "dataframe"
  )
  if (!any(manufacturer %in% manufacturer_possible)) {
    cli::cli_abort(c(
      "x" = "{.var manufacturer} needs to be one of {.val {manufacturer_possible}}"
    ))
  }

  if (manufacturer == "datapackage") {
    tag <- tag_create_datapackage(
      id,
      directory = directory,
      pressure_file = pressure_file,
      light_file = light_file,
      acceleration_file = acceleration_file,
      temperature_external_file = temperature_external_file,
      temperature_internal_file = temperature_internal_file,
      magnetic_file = magnetic_file,
      quiet = quiet
    )
  } else if (manufacturer == "soi") {
    tag <- tag_create_soi(
      id,
      directory = directory,
      pressure_file = pressure_file,
      light_file = light_file,
      acceleration_file = acceleration_file,
      temperature_external_file = temperature_external_file,
      temperature_internal_file = temperature_internal_file,
      magnetic_file = magnetic_file,
      quiet = quiet
    )
  } else if (manufacturer == "migratetech") {
    tag <- tag_create_migratetech(
      id,
      directory = directory,
      deg_file = pressure_file,
      light_file = light_file,
      quiet = quiet
    )
  } else if (manufacturer == "bas") {
    tag <- tag_create_bas(
      id,
      directory = directory,
      lig_file = light_file,
      quiet = quiet
    )
  } else if (manufacturer == "lund") {
    tag <- tag_create_lund(
      id,
      directory = directory,
      pressure_file = pressure_file,
      acceleration_light_file = acceleration_file,
      quiet = quiet
    )
  } else if (manufacturer == "prestag") {
    tag <- tag_create_prestag(
      id,
      directory = directory,
      pressure_file = pressure_file,
      quiet = quiet
    )
  } else if (manufacturer == "dataframe") {
    tag <- tag_create_dataframe(
      id,
      directory = directory,
      pressure_file = pressure_file,
      light_file = light_file,
      acceleration_file = acceleration_file,
      temperature_external_file = temperature_external_file,
      temperature_internal_file = temperature_internal_file,
      magnetic_file = magnetic_file,
      quiet = quiet
    )
  }

  if (assert_pressure) {
    tag_assert(tag, "pressure")
  }

  # Crop date
  tag <- tag_create_crop(tag, crop_start = crop_start, crop_end = crop_end, quiet = quiet)

  return(tag)
}

# Detect full path from the argument file.
#' @noRd
tag_create_detect <- function(file, directory, quiet = TRUE) {
  if (is.null(file)) {
    return(NULL)
  }
  if (is.na(file)) {
    return(NULL)
  }
  if (file.exists(file)) {
    return(file)
  }

  # Find files in directory ending with `file`
  path <- list.files(directory,
    pattern = glue::glue(file, "$"),
    full.names = TRUE
  )

  # Remove temporary file and those with word "test"
  path <- path[!grepl("~\\$|test", path, ignore.case = TRUE)]
  path <- path[!grepl("(?i)(?<!mag)calib", path, perl = TRUE)]

  if (length(path) == 0) {
    if (!quiet) {
      cli::cli_warn(c(
        "!" = glue::glue("No file is matching '", file, "'."),
        ">" = "This sensor will be ignored."
      ))
    }
    return(NULL)
  }
  if (length(path) > 1) {
    cli::cli_warn(c(
      "!" = "Multiple files matching {.var {file}}: {.file {path}}",
      ">" = "The function will continue with the first one."
    ))
    return(path[1])
  }
  return(path)
}



#' Read data file with a DTO format (Date Time Observation)
#'
#' @param sensor_path Full path of the file (directory + file)
#' @param skip Number of lines of the data file to skip before beginning to read data.
#' @param colIndex of the column of the data to take as observation.
#' @param date_format Format of the date (see [`strptime()`]).
#' @noRd
tag_create_dto <- function(sensor_path,
                           skip = 6,
                           col = 3,
                           date_format = "%d.%m.%Y %H:%M",
                           quiet = FALSE) {
  data_raw <- utils::read.delim(sensor_path, skip = skip, sep = "", header = FALSE)
  df <- data.frame(
    date = as.POSIXct(strptime(paste(data_raw[, 1], data_raw[, 2]),
      tz = "UTC",
      format = date_format
    )),
    value = data_raw[, col]
  )

  if (any(is.na(df$value))) {
    cli::cli_abort(c(
      x = "Invalid data in {.file {sensor_path)} at line(s): {skip + which(is.na(df$value))}",
      i = "Check and fix the corresponding lines"
    ))
  }

  if (!quiet) {
    cli::cli_bullets(c("v" = "Read {.file {sensor_path}}"))
  }
  return(df)
}

#' Read data file with a CSV format
#' @noRd
tag_create_csv <- function(sensor_path, col_name, quiet = FALSE) {
  df <- utils::read.csv(sensor_path)

  # Check if all specified columns are present
  missing_cols <- setdiff(col_name, names(df))
  if (length(missing_cols) > 0) {
    cli::cli_abort("The following columns are missing in {.file {sensor_path}}: \\
                              {glue::glue_collapse(missing_cols, ', ')}")
  }

  # Rename column datetime to date and convert to posixct
  names(df)[names(df) == "datetime"] <- "date"
  df$date <- as.POSIXct(df$date, format = "%Y-%m-%dT%H:%M", tz = "UTC")
  if (any(is.na(df$date))){
    df$date <- as.POSIXct(strptime(df$date, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  }
  if (any(is.na(df$date))){
    cli::cli_abort(c(
      x = "Invalid date in {.file {sensor_path}} at line(s): {which(is.na(df$date))}",
      i = "Check and fix the corresponding lines"
    ))
  }

  if (!quiet) {
    cli::cli_bullets(c("v" = "Read {.file {sensor_path}}"))
  }

  return(df)
}


#' Crop sensor data.frame
#' @noRd
tag_create_crop <- function(tag,
                            crop_start,
                            crop_end,
                            quiet = TRUE) {
  for (sensor in c(
    "pressure", "light", "acceleration", "temperature_internal",
    "temperature_external", "magnetic"
  )) {
    if (sensor %in% names(tag)) {
      # Crop time
      if (!is.null(crop_start)) {
        tag[[sensor]] <- tag[[sensor]][tag[[sensor]]$date >= as.POSIXct(crop_start, tz = "UTC"), ]
      }
      if (!is.null(crop_end)) {
        tag[[sensor]] <- tag[[sensor]][tag[[sensor]]$date < as.POSIXct(crop_end, tz = "UTC"), ]
      }

      if (!quiet) {
        # Check irregular time
        if (length(unique(diff(tag[[sensor]]$date))) > 1) {
          # nolint start
          dtime <- as.numeric(diff(tag[[sensor]]$date))
          cli::cli_warn("Irregular time spacing for {.field {sensor}}: \\
                  {tag[[sensor]]$date[which(dtime != dtime[1])]}.")
          # nolint end
        }

        if (nrow(tag[[sensor]]) == 0) {
          cli::cli_warn(c(
            "!" = "Empty {.field {sensor}} sensor dataset",
            ">" = "Check crop date."
          ))
        }
      }
    }
  }

  # Add parameter information
  tag$param$tag_create$crop_start <- crop_start
  tag$param$tag_create$crop_end <- crop_end

  tag
}
