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
#' You can also enter the data manually (`manufacturer = "manual"`) by providing the data.frame to
#' `pressure_file`:
#'   - `pressure_file`: data.frame with column date and value.
#'   - `light_file`: (optional) data.frame with column date and value.
#'   - `acceleration_file`: (optional) data.frame with column date and value.
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
#' @param manufacturer One of `NULL`, `"soi"`, `"migratetech"`, `"bas"`, `"lund"`, `"prestag"` or
#' `"manual"`.
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
      manufacturer <- "manual"
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
    "auto", "datapackage", "soi", "migratetech", "bas", "prestag",
    "lund", "manual"
  )
  if (!any(manufacturer %in% manufacturer_possible)) {
    cli::cli_abort(c(
      "x" = "{.var manufacturer} needs to be one of {.val {manufacturer_possible}}"
    ))
  }

  # Create tag
  tag <- structure(list(param = param_create(id = id)), class = "tag")

  if (manufacturer == "datapackage") {
    tag <- tag_create_dp(
      tag,
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
      tag,
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
      tag,
      directory = directory,
      deg_file = pressure_file,
      light_file = light_file,
      quiet = quiet
    )
  } else if (manufacturer == "bas") {
    tag <- tag_create_bas(
      tag,
      directory = directory,
      lig_file = light_file,
      quiet = quiet
    )
  } else if (manufacturer == "lund") {
    tag <- tag_create_lund(
      tag,
      directory = directory,
      pressure_file = pressure_file,
      acceleration_light_file = acceleration_file,
      quiet = quiet
    )
  } else if (manufacturer == "prestag") {
    tag <- tag_create_prestag(
      tag,
      directory = directory,
      pressure_file = pressure_file,
      quiet = quiet
    )
  } else if (manufacturer == "manual") {
    tag <- tag_create_manual(
      tag,
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

  ## Crop date
  tag <- tag_create_crop(tag, crop_start = crop_start, crop_end = crop_end, quiet = quiet)

  # Add parameter information
  tag$param$tag_create$manufacturer <- manufacturer
  tag$param$tag_create$crop_start <- crop_start
  tag$param$tag_create$crop_end <- crop_end
  tag$param$tag_create$directory <- directory

  return(tag)
}


# Read GeoLocator Data Package files
#' @noRd
tag_create_dp <- function(tag,
                          directory,
                          pressure_file = NULL,
                          light_file = NULL,
                          acceleration_file = NULL,
                          temperature_external_file = NULL,
                          temperature_internal_file = NULL,
                          magnetic_file = NULL,
                          quiet) {
  # Read Pressure
  if (is.null(pressure_file)) {
    pressure_path <- "pressure.csv"
  }
  pressure_path <- file.path(directory, "pressure.csv")
  if (file.exists(pressure_path)) {
    tag$pressure <- tag_create_csv(pressure_path,
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
    tag$light <- tag_create_csv(light_path,
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
    tag$acceleration <- tag_create_csv(acceleration_path,
      col_name = c("datetime", "value", "pitch"),
      quiet = quiet
    )
  }

  # Read external temperature
  if (is.null(temperature_external_file)) {
    temperature_external_file <- "temperature_external.csv"
  }
  temperature_external_path <- file.path(directory, temperature_external_file)
  if (file.exists(temperature_external_path)) {
    tag$temperature_external <- tag_create_csv(temperature_external_path,
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
    tag$temperature_internal <- tag_create_csv(temperature_internal_path,
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
        "datetime", "acceleration_x", "acceleration_y", "acceleration_z", "magnetic_x",
        "magnetic_y", "magnetic_z"
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

  return(tag)
}


# Read Swiss Ornithological Institute (SOI) tag files
#' @noRd
tag_create_soi <- function(tag,
                           directory,
                           pressure_file = NULL,
                           light_file = NULL,
                           acceleration_file = NULL,
                           temperature_external_file = NULL,
                           temperature_internal_file = NULL,
                           magnetic_file = NULL,
                           quiet) {
  # Read Pressure
  if (is.null(pressure_file)) {
    pressure_path <- tag_create_detect("*.pressure", directory, quiet = TRUE)
  } else {
    pressure_path <- tag_create_detect(pressure_file, directory, quiet = quiet)
  }
  if (!is.null(pressure_path)) {
    tag$pressure <- tag_create_dto(pressure_path, quiet = quiet)
  }

  # Read light
  if (is.null(light_file)) {
    light_path <- tag_create_detect("*.glf", directory, quiet = TRUE)
  } else {
    light_path <- tag_create_detect(light_file, directory, quiet = quiet)
  }
  if (!is.null(light_path)) {
    tag$light <-
      tag_create_dto(light_path, quiet = quiet)
  }

  # Read acceleration
  if (is.null(acceleration_file)) {
    acceleration_path <- tag_create_detect("*.acceleration", directory, quiet = TRUE)
  } else {
    acceleration_path <- tag_create_detect(acceleration_file, directory, quiet = quiet)
  }
  if (!is.null(acceleration_path)) {
    tag$acceleration <- tag_create_dto(acceleration_path, col = c(4, 3), quiet = quiet)
    names(tag$acceleration) <- c("date", "value", "pitch")
  }

  # Read External temperature
  if (is.null(temperature_external_file)) {
    temperature_external_path <- tag_create_detect("*.temperature", directory, quiet = TRUE)
    if (is.null(temperature_external_path)) {
      temperature_external_path <- tag_create_detect("*.airtemperature", directory, quiet = TRUE)
    }
  } else {
    temperature_external_path <- tag_create_detect(temperature_external_file, directory,
      quiet = quiet
    )
  }
  if (!is.null(temperature_external_path)) {
    tag$temperature_external <- tag_create_dto(temperature_external_path, col = 3, quiet = quiet)
  }

  # Read Internal temperature
  if (is.null(temperature_internal_file)) {
    temperature_internal_path <- tag_create_detect("*.bodytemperature", directory, quiet = TRUE)
  } else {
    temperature_internal_path <- tag_create_detect(temperature_internal_file, directory,
      quiet = quiet
    )
  }
  if (!is.null(temperature_internal_path)) {
    tag$temperature_internal <- tag_create_dto(temperature_internal_path, col = 3, quiet = quiet)
  }

  # Read magnetism
  if (is.null(magnetic_file)) {
    magnetic_path <- tag_create_detect("*.magnetic", directory, quiet = TRUE)
  } else {
    magnetic_path <- tag_create_detect(magnetic_file, directory, quiet = quiet)
  }
  if (!is.null(magnetic_path)) {
    tag$magnetic <- tag_create_dto(magnetic_path, col = seq(4, 9), quiet = quiet)
    names(tag$magnetic) <- c(
      "date", "acceleration_x", "acceleration_y", "acceleration_z",
      "magnetic_x", "magnetic_y", "magnetic_z"
    )
  }

  # Add parameter information
  tag$param$tag_create$pressure_file <- pressure_path
  tag$param$tag_create$light_file <- light_path
  tag$param$tag_create$acceleration_file <- acceleration_path
  tag$param$tag_create$temperature_external_file <- temperature_external_path
  tag$param$tag_create$temperature_internal_file <- temperature_internal_path
  tag$param$tag_create$magnetic_file <- magnetic_path

  tryCatch(
    {
      setting_path <- tag_create_detect("*.settings", directory, quiet = TRUE)
      if (!is.null(setting_path)) {
        tag$param$soi_settings <- jsonlite::fromJSON(setting_path)

        # Check for drift
        stop_time_ref <- as.POSIXct(strptime(tag$param$soi_settings$StopTimeReference,
          tz = "UTC",
          format = "%d.%m.%Y %H:%M:%S"
        ))
        stop_time_rtc <- as.POSIXct(strptime(tag$param$soi_settings$StopTimeRTC,
          tz = "UTC",
          format = "%d.%m.%Y %H:%M:%S"
        ))

        tag$param$drift <- abs(as.numeric(difftime(stop_time_ref, stop_time_rtc, units = "mins")))
        if (tag$param$drift > 30) {
          cli::cli_warn(c(
            "!" = "The SOI setting file {.file {setting_path}} is recording a drift of \\
            {round(tag$param$drift)} min which seems suspicious.",
            ">" = "Check for error (e.g. timezone)"
          ))
        }
      }
    },
    error = function(e) {
      cli::cli_alert_warning("Failed to load the SOI setting file {.file {setting_path}}.")
      message(e$message)
    }
  )

  return(tag)
}



# Read Migrate Technology tag files
#' @noRd
tag_create_migratetech <- function(tag,
                                   directory,
                                   deg_file = NULL,
                                   light_file = NULL,
                                   quiet) {
  # Read Pressure
  if (is.null(deg_file)) {
    deg_file <- "*.deg"
  }
  deg_path <- tag_create_detect(deg_file, directory, quiet = quiet)
  if (is.null(deg_path)) {
    cli::cli_abort(c(
      "x" = "There are no file {.val {deg_file}}",
      "!" = "{.var deg_file} is required!"
    ))
  }
  assertthat::assert_that(grepl("Migrate Technology", readLines(deg_path, n = 1)))
  line2 <- readLines(deg_path, n = 2)[[2]]
  v <- regmatches(line2, regexpr("Type: \\K\\d+", line2, perl = TRUE))
  if (v < 13) {
    cli::cli_abort(
      "The deg file {.file {deg_path}} is not compatible. Line 2 should \\
               contains {.val Type:x}, with x>=13."
    )
  }
  # Retrieve full model number
  tag$param$migratec_model <- regmatches(line2, regexpr("Type: \\K[\\d.]+", line2, perl = TRUE))
  # Check for drift
  line16 <- readLines(deg_path, n = 16)[[16]]
  drift <- abs(as.numeric(regmatches(line16, regexpr("-?\\d+\\.\\d*", line16))) / 60)
  if (drift > 30) {
    cli::cli_warn(c(
      "!" = "The deg file {.file {deg_path}} is recording a drift of {round(drift)} min \\
      (line 16) which seems suspicious.",
      ">" = "Check for error (e.g. timezone)"
    ))
  }
  # Find column index with pressure
  hdr <- utils::read.delim(deg_path, skip = 19, nrow = 1, header = FALSE, sep = "")
  col <- which(hdr == "P(Pa)")
  if (!(col > 0)) {
    cli::cli_abort(
      "The pressure file {.file {deg_path}} is not compatible. Line 20 \\
              should contains {.val P(Pa)}"
    )
  }
  # Read file
  tag$pressure <- tag_create_dto(deg_path,
    skip = 20, col = col,
    date_format = "%d/%m/%Y %H:%M:%S",
    quiet = quiet
  )
  # convert Pa in hPa
  tag$pressure$value <- tag$pressure$value / 100

  # Read acceleration
  col <- which(hdr == "Zact")
  if (length(col) != 0) {
    # Read file
    tag$acceleration <- tag_create_dto(deg_path,
      skip = 20, col = col,
      date_format = "%d/%m/%Y %H:%M:%S",
      quiet = quiet
    )
    tag$param$tag_create$acceleration_file <- deg_path
  }

  # Read temperature
  col <- which(hdr == "T('C)")
  if (length(col) != 0) {
    tag$temperature <- tag_create_dto(deg_path,
      skip = 20, col = col,
      date_format = "%d/%m/%Y %H:%M:%S",
      quiet = quiet
    )
    tag$param$tag_create$temperature_external_file <- deg_path
  }

  # Read light
  if (is.null(light_file)) {
    light_path <- tag_create_detect("*.lux", directory, quiet = TRUE)
  } else {
    light_path <- tag_create_detect(light_file, directory, quiet = quiet)
  }
  if (!is.null(light_path)) {
    line16 <- readLines(light_path, n = 16)[[16]]
    drift <- abs(as.numeric(regmatches(line16, regexpr("-?\\d+\\.\\d*", line16))) / 60)
    if (drift > 30) {
      cli::cli_warn(c(
        "!" = "The light file {.file {light_path}} is recording a drift of {round(drift)} min \\
      (line 16) which seems suspicious.",
        ">" = "Check for error (e.g. timezone)"
      ))
    }
    # find column index with light
    hdr <- utils::read.delim(light_path, skip = 19, nrow = 1, header = FALSE, sep = "")
    col <- which(hdr == "light(lux)")
    if (length(col) == 0) {
      cli::cli_abort(
        "The light file {.file {light_path}} is not compatible. Line 20 \\
              should contains {.val light(lux)}"
      )
    }
    # Read file
    tag$light <- tag_create_dto(light_path,
      skip = 20, col = col,
      date_format = "%d/%m/%Y %H:%M:%S",
      quiet = quiet
    )

    if (drift > 5) {
      cli::cli_warn(c(
        "!" = "The light file {.file {light_path}} is recording a drift of {round(drift)} min \\
      (line 16) which is higher than the resolution .",
        "i" = "Check for error (e.g. timezone)"
      ))
    }
  }

  # Add parameter information
  tag$param$tag_create$pressure_file <- deg_path
  tag$param$tag_create$light_file <- light_path

  return(tag)
}


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
tag_create_bas <- function(tag,
                           directory,
                           lig_file = NULL,
                           act_file = NULL,
                           quiet) {
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
    date = as.POSIXct(strptime(data_raw[, 2],
      tz = "UTC",
      format = "%d/%m/%y %H:%M:%S"
    )),
    value = data_raw[, 4]
  )

  # Add parameter information
  tag$param$tag_create$light_file <- lig_file

  return(tag)
}



# Read Lund tag files
#' @noRd
tag_create_lund <- function(tag,
                            directory,
                            pressure_file = NULL,
                            acceleration_light_file = NULL,
                            quiet) {
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
  if (!quiet) {
    cli::cli_bullets(c("v" = "Read {.file {pressure_path}}"))
  }


  # Read light
  if (is.null(acceleration_light_file)) {
    acc_light_path <- tag_create_detect("_acc.xlsx", directory, quiet = TRUE)
  } else {
    acc_light_path <- tag_create_detect(acceleration_light_file, directory, quiet = quiet)
  }
  if (!is.null(acc_light_path)) {
    xls <- readxl::read_excel(acc_light_path,
      sheet = "Light", skip = 1,
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

  return(tag)
}

# Read PresTag tag files
#' @noRd
tag_create_prestag <- function(tag,
                               directory,
                               pressure_file = NULL,
                               quiet) {
  # Find file path
  if (is.null(pressure_file)) {
    pressure_file <- ".txt"
  }
  pressure_path <- tag_create_detect(pressure_file, directory, quiet = quiet)
  if (is.null(pressure_path)) {
    cli::cli_abort(c(
      "x" = "There are no file {.val {pressure_path}}",
      "!" = "{.var pressure_path} is required"
    ))
  }

  # Read
  data_raw <- read.delim(pressure_path, header = FALSE, comment.char = "#", sep = ",")

  # convert epoch to Posixt
  timestamps <- as.POSIXct(data_raw$V1, origin = "1970-01-01", tz = "UTC")

  # Separate pressure and temperature
  df <- read.table(text = data_raw$V2, sep = ":", col.names = c("sensor", "value"))
  df$date <- timestamps

  # df2 <- read.table(text = data_raw$V3[data_raw$V3!=""],
  #                  sep = ":", col.names = c("sensor", "value"))
  # df2$date = timestamps[data_raw$V3!=""]
  # df = rbind(df, df2)

  # Set to NA any negtive value
  df$value[df$value < 0] <- NA

  # Create sensor data.frame
  tag$pressure <- df[df$sensor == "P", -which(names(df) == "sensor")]
  tag$temperature <- df[df$sensor == "T", -which(names(df) == "sensor")]

  # Add parameter information
  tag$param$tag_create$pressure_file <- pressure_path

  return(tag)
}


# Read Migrate Technology tag files
#' @noRd
tag_create_manual <- function(tag,
                              directory,
                              pressure_file = NULL,
                              light_file = NULL,
                              acceleration_file = NULL,
                              temperature_external_file = NULL,
                              temperature_internal_file = NULL,
                              magnetic_file = NULL,
                              quiet) {
  # Read Pressure
  if (!is.null(pressure_file)) {
    assertthat::assert_that(is.data.frame(pressure_file))
    assertthat::assert_that(assertthat::has_name(pressure_file, c("date", "value")))
    assertthat::assert_that(inherits(pressure_file$date, "POSIXct"))
    assertthat::assert_that(assertthat::are_equal(attr(pressure_file$date, "tzone"), "UTC"))
    if (min(pressure_file$value, na.rm = TRUE) < 250 ||
      1100 < max(pressure_file$value, na.rm = TRUE)) {
      cli::cli_warn("Pressure observation should be between 250 hPa (~10000m) and 1100 hPa \\
    (sea level at 1013hPa). Check unit of pressure data.frame provided.")
    }
    tag$pressure <- pressure_file
    tag$param$tag_create$pressure_file <- "manual"
  }

  # Read light
  if (!is.null(light_file)) {
    assertthat::assert_that(assertthat::has_name(light_file, c("date", "value")))
    assertthat::assert_that(inherits(light_file$date, "POSIXct"))
    assertthat::assert_that(assertthat::are_equal(attr(light_file$date, "tzone"), "UTC"))
    tag$light <- light_file
    tag$param$tag_create$light_file <- "manual"
  }

  # Read acceleration
  if (!is.null(acceleration_file)) {
    assertthat::assert_that(assertthat::has_name(acceleration_file, c("date", "value")))
    assertthat::assert_that(inherits(acceleration_file$date, "POSIXct"))
    assertthat::assert_that(assertthat::are_equal(attr(acceleration_file$date, "tzone"), "UTC"))
    tag$acceleration <- acceleration_file
    tag$param$tag_create$acceleration_file <- "manual"
  }

  # Read acceleration
  if (!is.null(temperature_external_file)) {
    assertthat::assert_that(assertthat::has_name(temperature_external_file, c("date", "value")))
    assertthat::assert_that(inherits(temperature_external_file$date, "POSIXct"))
    assertthat::assert_that(assertthat::are_equal(
      attr(temperature_external_file$date, "tzone"),
      "UTC"
    ))
    tag$temperature <- temperature_external_file
    tag$param$tag_create$temperature_external_file <- "manual"
  }

  # Read air temperature
  if (!is.null(temperature_external_file)) {
    assertthat::assert_that(assertthat::has_name(temperature_external_file, c("date", "value")))
    assertthat::assert_that(inherits(temperature_external_file$date, "POSIXct"))
    assertthat::assert_that(assertthat::are_equal(
      attr(temperature_external_file$date, "tzone"),
      "UTC"
    ))
    tag$airtemperature <- temperature_external_file
    tag$param$tag_create$temperature_external_file <- "manual"
  }

  # Read magnetism
  if (!is.null(magnetic_file)) {
    assertthat::assert_that(assertthat::has_name(
      magnetic_file,
      c(
        "date", "acceleration_x", "acceleration_y", "acceleration_z", "magnetic_x", "magnetic_y",
        "magnetic_z"
      )
    ))
    assertthat::assert_that(inherits(magnetic_file$date, "POSIXct"))
    assertthat::assert_that(assertthat::are_equal(attr(magnetic_file$date, "tzone"), "UTC"))
    tag$magnetic <- magnetic_file
    tag$param$tag_create$magnetic_file <- "manual"
  }

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
  path <- path[!grepl("~\\$|test|calib", path, ignore.case = TRUE)]

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
    cli::cli_abort(glue::glue("The following columns are missing in {.file {sensor_path}}:\\
                              {glue::glue_collapse(missing_cols, ', ')}"))
  }

  # Rename column datetime to date and convert to posixct
  names(df)[names(df) == "datetime"] <- "date"
  df$date <- as.POSIXct(strptime(df$date, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))

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
                            quiet) {
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
  return(tag)
}
