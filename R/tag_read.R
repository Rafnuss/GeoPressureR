#' Read tag data
#'
#' @description
#' Imports multi-sensor logger data from a folder (`directory`) and optionally crops at specific
#' date. The `*_file` arguments are matched using a regex expression (e.g., `"*.pressure"`
#' matches any file with the extension `pressure`).
#'
#' The current implementation can read the files from:
#' - [Swiss Ornithological Institute (SOI)
#' ](https://www.vogelwarte.ch/en/projects/bird-migration/tracking-devices-miniaturized-geolocators)
#'  (default) with `pressure_file = "*.pressure"`, `light_file = "*.glf"` and
#'  `acceleration_file = "*.acceleration"`
#' - [Migrate Technology](http://www.migratetech.co.uk/) with `pressure_file = "*.deg"`,
#' `light_file = "*.lux"` and `acceleration_file = "*.deg"`
#'
#' Please create [an issue on Github](https://github.com/Rafnuss/GeoPressureR/issues/new) if you have data
#' in a format not supported yet.
#'
#' @param id Unique identifier of the tag (character)
#' @param directory Path of the directory where the tag files can be read
#' @param pressure_file Name of the file with pressure data. Extension must be `.pressure` or
#' `.deg` (required).
#' @param light_file Name of the file with light data. Extension must be `.glf`, `.lux` (or `NA`
#' to ignore).
#' @param acceleration_file Name of the file with acceleration data. Extension must be
#' `.acceleration`, `.deg` (or `NA` to ignore).
#' @param crop_start Remove all data before this date (POSIXct or character in UTC).
#' @param crop_end Remove all data after this date (POSIXct or character in UTC).
#' @return A list containing
#' - `id` character of the unique identifier of the tag
#' - `pressure` data.frame with column `date` and `value`
#' - `light` (optional) same structure as pressure
#' - `acceleration` (optional) same structure as pressure
#' @seealso [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#read-geolocator-data)
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#'
#' tag <- tag_read("18LX")
#' str(tag)
#'
#' tag <- tag_read("18LX", crop_start = "2017-08-01", crop_end = "2017-08-05")
#' str(tag)
#'
#' # For Migrate Technology file, use
#' tag <- tag_read("CB621",
#'   pressure_file = "*.deg",
#'   light_file = "*.lux",
#'   acceleration_file = NA
#' )
#' str(tag)
#'
#' # You can also specify exactly the file in case multiple file with the same
#' # extension exist in your directory
#' tag <- tag_read("CB621",
#'   pressure_file = "CB621_BAR.deg",
#'   light_file = NA,
#'   acceleration_file = NA
#' )
#' str(tag)
#'
#' @export
tag_read <- function(id,
                     directory = glue::glue("data/0-tag/{id}/"),
                     pressure_file = "*.pressure",
                     light_file = "*.glf",
                     acceleration_file = "*.acceleration",
                     crop_start = "1900-01-01",
                     crop_end = "2100-01-01") {
  assertthat::assert_that(is.character(id))
  assertthat::assert_that(assertthat::is.dir(directory))

  # convert date to POSIXct date and check format
  crop_start <- as.POSIXct(crop_start, tz = "UTC")
  crop_end <- as.POSIXct(crop_end, tz = "UTC")

  # Find the exact filename
  sensor_paths <- sapply(c(pressure_file, light_file, acceleration_file), function(f) {
    if (is.na(f)) {
      return(NA)
    }
    path <- list.files(directory, pattern = glue::glue(f, "$"), full.names = TRUE)
    if (length(path) == 0) {
      cli::cli_warn(c(
        "!" = glue::glue("No file is matching '", f, "'."),
        ">" = "This file will be ignored."
      ))
      path <- ""
    } else if (length(path) > 1) {
      cli::cli_warn(c(
        "!" = "Multiple files matching {.file {f}}: {path}",
        ">" = "The function will continue with the first one."
      ))
      path <- path[1]
    }
    return(path)
  })

  tag <- structure(list(
    id = id
  ), class="tag")

  # Read Pressure
  tag$pressure <- switch(tools::file_ext(sensor_paths[1]),
    "pressure" = {
      subset(tag_read_dto(sensor_paths[1]), date >= crop_start & date < crop_end)
    },
    "deg" = {
      # Check that it is a valid Migrate Technology file
      assertthat::assert_that(grepl("Migrate Technology", readLines(sensor_paths[1], n = 1)))
      line2 <- readLines(sensor_paths[1], n = 2)[2]
      v <- regmatches(line2, regexpr("Type: \\K\\d+", line2, perl = TRUE))
      if (v < 13) {
        cli::cli_abort(
          "The pressure file {.file {sensor_paths[1]}} is not compatible. Line 2 should \\
             contains {.val Types:x}, with x>=13."
        )
      }

      # find column index with pressure
      col <- which(utils::read.delim(sensor_paths[1],
        skip = 19, nrow = 1, header = FALSE, sep = ""
      ) == "P(Pa)")
      if (!(col > 0)) {
        cli::cli_abort(
          "The pressure file {.file {sensor_paths[1]}} is not compatible. Line 20 \\
            should contains {.val P(Pa)}"
        )
      }

      # Read file
      pres <- tag_read_dto(sensor_paths[1],
        skip = 20, col = col,
        date_format = "%d/%m/%Y %H:%M:%S"
      )

      # convert Pa in hPa
      pres$value <- pres$value / 100

      # Crop time
      subset(pres, date >= crop_start & date < crop_end)
    },
    {
      cli::cli_abort(
        "The pressure file {.file {sensor_paths[1]}} should have the extension {.file .pressure} or \\
        {.file .deg}"
      )
    }
  )

  # Read light
  if (!is.na(sensor_paths[2])) {
    tag$light <- switch(tools::file_ext(sensor_paths[2]),
      "glf" = {
        subset(tag_read_dto(sensor_paths[2]), date >= crop_start & date < crop_end)
      },
      "lux" = {
        # find column index with light
        col <- which(utils::read.delim(
          sensor_paths[2],
          skip = 19, nrow = 1, header = FALSE, sep = ""
        ) == "light(lux)")
        if (!(col > 0)) {
          cli::cli_abort(
            "The light file {.file {sensor_paths[2]}} is not compatible. Line 20 \\
            should contains {.val light(lux)}"
          )
        }

        # Read file
        light <- tag_read_dto(sensor_paths[2],
          skip = 20, col = col,
          date_format = "%d/%m/%Y %H:%M:%S"
        )

        subset(light, date >= crop_start & date < crop_end)
      }
    )
  }

  # Read acceleration
  if (!is.na(sensor_paths[3])) {
    tag$acceleration <- switch(tools::file_ext(sensor_paths[3]),
      "acceleration" = {
        subset(tag_read_dto(sensor_paths[3], col = 4), date >= crop_start & date < crop_end)
      },
      "deg" = {
        # Check that it is a valid Migrate Technology file
        assertthat::assert_that(grepl("Migrate Technology", readLines(sensor_paths[3], n = 1)))
        line2 <- readLines(sensor_paths[3], n = 2)[2]
        v <- regmatches(line2, regexpr("Type: \\K\\d+", line2, perl = TRUE))
        if (v < 13) {
          cli::cli_abort(
            "The acceleration file {.file {sensor_paths[3]}} is not compatible. Line 2 should \\
             contains {.val Types:x}, with x>=13."
          )
        }

        # find column index with acceleration
        col <- which(utils::read.delim(sensor_paths[3],
          skip = 19, nrow = 1, header = FALSE, sep = ""
        ) == "Zact")
        if (!(col > 0)) {
          cli::cli_abort(
            "The light file {.file {sensor_paths[3]}} is not compatible. Line 20 \\
            should contains {.val Zact}"
          )
        }

        # Read file
        acc <- tag_read_dto(sensor_paths[3],
          skip = 20, col = col,
          date_format = "%d/%m/%Y %H:%M:%S"
        )
        # Crop time
        subset(acc, date >= crop_start & date < crop_end)
      }
    )
  }

  return(tag)
}

#' Read data file with a DTO format (Date Time Observation)
#'
#' @param sensor_path Full path of the file (directory + file)
#' @param skip Number of lines of the data file to skip before beginning to read data.
#' @param colIndex of the column of the data to take as observation.
#' @param date_format Format of the date (see [`strptime()`]).
#' @seealso [`tag_read()`]
#' @noRd
tag_read_dto <- function(sensor_path, skip = 6, col = 3, date_format = "%d.%m.%Y %H:%M") {
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
      x = "Invalid data in {.file {sensor_path)} at line(s): {20 + which(is.na(df$value))}",
      i = "Check and fix the corresponding lines"
    ))
  }

  if (length(unique(diff(df$date))) > 1) {
    dtime <- as.numeric(diff(df$date))
    cli::cli_warn("Irregular time spacing in {.file {sensor_path}} at line(s): \\
                  {20 + which(dtime != dtime[1])}.")
  }
  cli::cli_alert_success("Read {sensor_path}")
  return(df)
}
