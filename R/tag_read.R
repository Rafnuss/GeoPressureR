#' Read tag data
#'
#' @description
#' Imports multi-sensor logger data from a folder (`directory`) and optionally crop at specific
#' date. The `*_filename` arguments are matched using a regex expression (e.g., `"*.pressure"`
#' matches any files with the extension `pressure`).
#'
#' The current implementation can read the files from:
#' - [Swiss Ornithological Institute (SOI)
#' ](https://www.vogelwarte.ch/en/projects/bird-migration/tracking-devices-miniaturized-geolocators)
#'  (default) with `pressure_filename = "*.pressure"`, `light_filename = "*.glf"` and
#'  `acceleration_filename = "*.acceleration"`
#' - [Migrate Technology](http://www.migratetech.co.uk/) with `pressure_filename = "*.deg"`,
#' `light_filename = "*.lux"` and `acceleration_filename = "*.deg"`
#'
#' Create [an issue on github](https://github.com/Rafnuss/GeoPressureR/issues/new) if you have data
#' in a format not supported yet.
#'
#' @param directory Path of the directory where the files are stored
#' @param pressure_filename Name of the file with pressure data. Extension must be `.pressure` or
#' `.deg` (required).
#' @param light_filename Name of the file with light data. Extension must be `.glf`, `.lux` or `NA`
#' if absent.
#' @param acceleration_filename Name of the file with acceleration data. Extension must be
#' `.acceleration`, `.deg` or `NA` if absent.
#' @param crop_start Remove all data before this date (POSIXct in UTC).
#' @param crop_end Remove all data after this date (POSIXct in UTC).
#' @param id Unique identifier of the tag Default (`NA`) is to take the part of
#' `pressure_filename` up to a character `_` (e.g. `18LX` for `18LX_20180725.pressure`). If
#' `id = "basename"`, take the [`basename()`] of `pressure_filename`
#' @return a list containing (see examples)
#' - `id` character of the unique identifier of the tag
#' - `pressure` data.frame with column `date` and `value`
#' - `light` (optional) same structure as pressure
#' - `acceleration` (optional) same structure as pressure
#' @seealso [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#read-geolocator-data)
#' @examples
#' # By default, the function will read the files "18LX_20180725.pressure",
#' # "18LX_20180725.glf" and "18LX_20180725.acceleration" from SOI standard
#' tag <- tag_read(
#'   directory = system.file("extdata/0_tag/18LX", package = "GeoPressureR")
#' )
#' str(tag)
#'
#' # For Migrate Technology file, use
#' tag <- tag_read(
#'   directory = system.file("extdata/0_tag/CB621", package = "GeoPressureR"),
#'   pressure_filename = "*.deg",
#'   light_filename = "*.lux",
#'   acceleration_filename = NA
#' )
#' str(tag)
#'
#' # You can also specify exactly the filename in case multiple file with the same
#' # extension exist in your directory
#' tag <- tag_read(
#'   directory = system.file("extdata/0_tag/CB621", package = "GeoPressureR"),
#'   pressure_filename = "CB621_BAR.deg",
#'   light_filename = NA,
#'   acceleration_filename = NA
#' )
#' str(tag)
#'
#' @export
tag_read <- function(directory,
                     pressure_filename = "*.pressure",
                     light_filename = "*.glf",
                     acceleration_filename = "*.acceleration",
                     crop_start = "1900-01-01",
                     crop_end = "2100-01-01",
                     id = NA) {
  assertthat::assert_that(dir.exists(directory))

  # convert date to POSIXct date and check format
  crop_start <- as.POSIXct(crop_start, tz = "UTC")
  crop_end <- as.POSIXct(crop_end, tz = "UTC")

  # Check existence of the file
  pressure_path <- ifelse(is.na(pressure_filename), "",
    tag_read_check(directory, pressure_filename)
  )
  light_path <- ifelse(is.na(light_filename), "",
                       tag_read_check(directory, light_filename)
                       )
  acceleration_path <- ifelse(is.na(acceleration_filename), "",
    tag_read_check(directory, acceleration_filename)
  )

  if (is.na(id)) {
    id <- strsplit(basename(pressure_path), "_")[[1]][1]
  } else if (id == "basename") {
    id <- basename(directory)
  }

  # Initialize the tag list
  tag <- list(
    id = id,
    pressure = switch(tools::file_ext(pressure_path),
      "pressure" = {
        subset(tag_read_delim_dto(pressure_path), date >= crop_start & date < crop_end)
      },
      "deg" = {
        # Check that it is a valid Migrate Technology file
        assertthat::assert_that(grepl("Migrate Technology", readLines(pressure_path, n = 1)))
        line2 <- readLines(pressure_path, n = 2)[2]
        v <- regmatches(line2, regexpr("Type: \\K\\d+", line2, perl = TRUE))
        assertthat::assert_that(v >= 13,
          msg = paste0(
            "The pressure file (*.deg) is not compatible. Line 2 ",
            "should contains 'Types:x', with x>=13."
          )
        )

        # find column index with pressure
        col <- which(utils::read.delim(pressure_path,
          skip = 19, nrow = 1, header = FALSE, sep = ""
        ) == "P(Pa)")
        assertthat::assert_that(col > 0,
          msg = paste0(
            "The pressure file (*.deg) is not compatible. Line 20 ",
            "should contains 'P(Pa)'"
          )
        )

        # Read file
        pres <- tag_read_delim_dto(pressure_path,
          skip = 20, col = col,
          date_format = "%d/%m/%Y %H:%M:%S"
        )

        # Check for error
        if (any(is.na(pres$value))) {
          stop(paste0("Invalid data in ", basename(pressure_path), " at line(s): ", 20 +
            which(is.na(pres$value)), ". Check and fix the corresponding lines"))
        }
        if (length(unique(diff(pres$date))) > 1) {
          dtime <- as.numeric(diff(pres$date))
          warning(paste0(
            "Irregular time spacing in ", basename(pressure_path), " at line(s): ",
            20 + which(dtime != dtime[1]), "."
          ))
        }

        # convert Pa in hPa
        pres$value <- pres$value / 100

        # Crop time
        subset(pres, date >= crop_start & date < crop_end)
      },
      {
        data.frame()
      }
    ),
    light = switch(tools::file_ext(light_path),
      "glf" = {
        subset(tag_read_delim_dto(light_path), date >= crop_start & date < crop_end)
      },
      "lux" = {
        # find column index with light
        col <- which(utils::read.delim(
          light_path,
          skip = 19, nrow = 1, header = FALSE, sep = ""
        ) == "light(lux)")
        assertthat::assert_that(col > 0,
          msg = paste0(
            "The light file (*.lux) is not compatible. Line 20 should contains 'light(lux)'"
          )
        )

        # Read file
        light <- tag_read_delim_dto(light_path,
          skip = 20, col = col,
          date_format = "%d/%m/%Y %H:%M:%S"
        )

        # Check for error
        if (any(is.na(light$value))) {
          stop(paste0("Invalid data in ", basename(light_path), " at line(s): ", 20 +
            which(is.na(light$value)), ". Check and fix the corresponding lines"))
        }
        if (length(unique(diff(light$date))) > 1) {
          dtime <- as.numeric(diff(light$date))
          warning(paste0(
            "Irregular time spacing in ", basename(light_path), " at line(s): ",
            20 + which(dtime != dtime[1]), "."
          ))
        }
        subset(light, date >= crop_start & date < crop_end)
      },
      {
        data.frame()
      }
    ),
    acceleration = switch(tools::file_ext(acceleration_path),
      "acceleration" = {
        subset(tag_read_delim_dto(acceleration_path, col = 4), date >= crop_start & date < crop_end)
      },
      "deg" = {
        # Check that it is a valid Migrate Technology file
        assertthat::assert_that(grepl("Migrate Technology", readLines(acceleration_path, n = 1)))
        line2 <- readLines(acceleration_path, n = 2)[2]
        v <- regmatches(line2, regexpr("Type: \\K\\d+", line2, perl = TRUE))
        assertthat::assert_that(v >= 13,
          msg = paste0(
            "The acceleration file (*.deg) is not compatible. Line 2  should contains 'Types:x', ",
            "with x>=13."
          )
        )

        # find column index with acceleration
        col <- which(utils::read.delim(acceleration_path,
          skip = 19, nrow = 1, header = FALSE, sep = ""
        ) == "Zact")
        assertthat::assert_that(col > 0,
          msg = paste0(
            "The acceleration file (*.deg) is not compatible. Line 20 should contains 'Zact'"
          )
        )

        # Read file
        acc <- tag_read_delim_dto(acceleration_path,
          skip = 20, col = col,
          date_format = "%d/%m/%Y %H:%M:%S"
        )

        # Check for error
        if (any(is.na(acc$value))) {
          stop(paste0("Invalid data in ", basename(acceleration_path), " at line(s): ", 20 +
            which(is.na(acc$value)), ". Check and fix the corresponding lines"))
        }
        if (length(unique(diff(acc$date))) > 1) {
          dtime <- as.numeric(diff(acc$date))
          warning(paste0(
            "Irregular time spacing in ", basename(acceleration_path), " at line(s): ",
            20 + which(dtime != dtime[1]), "."
          ))
        }
        # Crop time
        subset(acc, date >= crop_start & date < crop_end)
      },
      {
        data.frame()
      }
    )
  )

  return(tag)
}

#' Check that a given `directory` and `filename` exists and is unique.
#'
#' @param directory Directory of the file
#' @param filename Name of the file
#' @seealso [`tag_read()`]
#' @noRd
tag_read_check <- function(directory, filename) {
  path <- list.files(directory, pattern = paste0(filename, "$"), full.names = TRUE)
  if (length(path) == 0) {
    warning(paste0("No file is matching '", filename, "'. This file will be ignored."))
    path <- ""
  } else if (length(path) > 1) {
    warning(paste0(
      "Multiple files matching '", filename, "': \n", paste(path, collapse = "\n"),
      ". \nThe function will continue with the first one."
    ))
    path <- path[1]
  }
  return(path)
}

#' Read data file with a DTO format (Date Time Observation)
#'
#' @param full_path Full path of the file (directory + filename)
#' @param skip Number of lines of the data file to skip before beginning to read data.
#' @param colIndex of the column of the data to take as observation.
#' @param date_format Format of the date (see [`strptime()`]).
#' @seealso [`tag_read()`]
#' @noRd
tag_read_delim_dto <- function(full_path, skip = 6, col = 3, date_format = "%d.%m.%Y %H:%M") {
  cli::cli_progress_step("Read {full_path}")
  data_raw <- utils::read.delim(full_path, skip = skip, sep = "", header = FALSE)
  df <- data.frame(
    date = as.POSIXct(strptime(paste(data_raw[, 1], data_raw[, 2]),
      tz = "UTC",
      format = date_format
    )),
    value = data_raw[, col]
  )
  return(df)
}
