#' Read `tag` data
#'
#' @description
#' Imports multi-sensor logger data from the folder `directory` and optionally crops at specific
#' date. The `*_file` arguments are matched using a regex expression (e.g., `"*.pressure"`
#' matches any file with the extension `pressure`).
#'
#' The current implementation can read the files from:
#' - [Swiss Ornithological Institute (SOI)
#' ](https://www.vogelwarte.ch/en/projects/bird-migration/tracking-devices-miniaturized-geolocators)
#'  (default)
#'    - `pressure_file = "*.pressure"`
#'    - `light_file = "*.glf"`
#'    - `acceleration_file = "*.acceleration"`
#' - [Migrate Technology](http://www.migratetech.co.uk/):
#'    - `pressure_file = "*.deg"`
#'    - `light_file = "*.lux"`
#'    - `acceleration_file = "*.deg"`
#'
#'  Use `NULL` to hide the message warning about the absence of a sensor file.
#'
#' Please create [an issue on Github](https://github.com/Rafnuss/GeoPressureR/issues/new) if you
#' have data in a format not supported yet.
#'
#' @param id Unique identifier of a tag.
#' @param directory Path of the directory where the tag files can be read
#' @param pressure_file Name of the file with pressure data. Extension must be `.pressure` or
#' `.deg` (required).
#' @param light_file Name of the file with light data. Extension must be `.glf`, `.lux` (or `NA`
#' to ignore).
#' @param acceleration_file Name of the file with acceleration data. Extension must be
#' `.acceleration`, `.deg` (or `NA` to ignore).
#' @param crop_start Remove all data before this date (POSIXct or character in UTC).
#' @param crop_end Remove all data after this date (POSIXct or character in UTC).
#' @param quiet Logical to hide the file name read. Default is `FALSE`.
#' @return A GeoPressureR `tag` object containing
#' - `id` character of the unique identifier of the tag
#' - `pressure` data.frame with column `date` and `value`
#' - `light` (optional) same structure as pressure
#' - `acceleration` (optional) same structure as pressure
#'
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#'
#' tag_create("18LX")
#'
#' tag_create("18LX", crop_start = "2017-08-01", crop_end = "2017-08-05")
#'
#' # For Migrate Technology file, use
#' tag_create("CB621",
#'   pressure_file = "*.deg",
#'   light_file = "*.lux",
#'   acceleration_file = NA
#' )
#'
#' # You can also specify exactly the file in case multiple file with the same
#' # extension exist in your directory
#' tag_create("CB621",
#'   pressure_file = "CB621_BAR.deg",
#'   light_file = NA,
#'   acceleration_file = NA
#' )
#'
#' @family tag
#' @seealso [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#read-geolocator-data)
#' @export
tag_create <- function(id,
                       directory = glue::glue("./data/raw-tag/{id}/"),
                       pressure_file = "*.pressure",
                       light_file = "*.glf",
                       acceleration_file = "*.acceleration",
                       crop_start = NULL,
                       crop_end = NULL,
                       quiet = FALSE) {
  # Create tag
  tag <- structure(list(
    param = param_create(id = id)
  ), class = "tag")

  # Fix NA vs NULL, so that both would work
  if (is.null(pressure_file)) {
    pressure_file <- NA
  }
  if (is.null(acceleration_file)) {
    acceleration_file <- NA
  }
  if (is.null(light_file)) {
    light_file <- NA
  }

  # Find the exact filename
  sensor_paths <- sapply(c(pressure_file, light_file, acceleration_file), function(f) {
    if (is.na(f)) {
      return(NA)
    }
    if (file.exists(f)) {
      return(f)
    }
    assertthat::assert_that(assertthat::is.dir(directory))
    path <- list.files(directory, pattern = glue::glue(f, "$"), full.names = TRUE)
    if (length(path) == 0) {
      cli::cli_warn(c(
        "!" = glue::glue("No file is matching '", f, "'."),
        ">" = "This file will be ignored."
      ))
      return(NA)
    }
    if (length(path) > 1) {
      cli::cli_warn(c(
        "!" = "Multiple files matching {.file {f}}: {path}",
        ">" = "The function will continue with the first one."
      ))
      return(path[1])
    }
    return(path)
  })

  # Read Pressure
  if (is.na(sensor_paths[[1]])) {
    cli::cli_abort(c(
      "x" = "There are no pressure file {.val {names(sensor_paths[1])}}",
      "!" = "Pressure file are required"
    ))
  } else if (tools::file_ext(sensor_paths[[1]]) == "pressure") {
    tag$pressure <- tag_create_dto(sensor_paths[[1]], quiet = quiet)
  } else if (tools::file_ext(sensor_paths[[1]]) == "deg") {
    # Check that it is a valid Migrate Technology file
    assertthat::assert_that(grepl("Migrate Technology", readLines(sensor_paths[[1]], n = 1)))
    line2 <- readLines(sensor_paths[[1]], n = 2)[[2]]
    v <- regmatches(line2, regexpr("Type: \\K\\d+", line2, perl = TRUE))
    if (v < 13) {
      cli::cli_abort(
        "The pressure file {.file {sensor_paths[1]}} is not compatible. Line 2 should \\
             contains {.val Types:x}, with x>=13."
      )
    }

    # find column index with pressure
    col <- which(utils::read.delim(sensor_paths[[1]],
      skip = 19, nrow = 1, header = FALSE, sep = ""
    ) == "P(Pa)")
    if (!(col > 0)) {
      cli::cli_abort(
        "The pressure file {.file {sensor_paths[[1]]}} is not compatible. Line 20 \\
            should contains {.val P(Pa)}"
      )
    }

    # Read file
    pres <- tag_create_dto(sensor_paths[[1]],
      skip = 20, col = col,
      date_format = "%d/%m/%Y %H:%M:%S",
      quiet = quiet
    )

    # convert Pa in hPa
    pres$value <- pres$value / 100
    tag$pressure <- pres
  } else {
    cli::cli_abort(
      "The pressure file {.val {sensor_paths[[1]]}} should have the extension {.val .pressure} \\
        or {.val .deg}"
    )
  }

  # Crop time
  if (!is.null(crop_start)) {
    # convert date to POSIXct date and check format
    crop_start <- as.POSIXct(crop_start, tz = "UTC")
    tag$pressure <- subset(tag$pressure, date >= crop_start)
  }
  if (!is.null(crop_end)) {
    crop_end <- as.POSIXct(crop_end, tz = "UTC")
    tag$pressure <- subset(tag$pressure, date < crop_end)
  }

  if (nrow(tag$pressure) == 0) {
    cli::cli_abort(c(
      "!" = "Empty {.field pressure} sensor dataset from {.file {sensor_paths[[1]]}}",
      ">" = "Check crop date."
    ))
  }


  # Read light
  if (!is.na(sensor_paths[[2]])) {
    tag$light <- switch(tools::file_ext(sensor_paths[[2]]),
      "glf" = {
        tag_create_dto(sensor_paths[[2]], quiet = quiet)
      },
      "lux" = {
        # find column index with light
        col <- which(utils::read.delim(
          sensor_paths[[2]],
          skip = 19, nrow = 1, header = FALSE, sep = ""
        ) == "light(lux)")
        if (!(col > 0)) {
          cli::cli_abort(
            "The light file {.file {sensor_paths[[2]]}} is not compatible. Line 20 \\
            should contains {.val light(lux)}"
          )
        }

        # Read file
        tag_create_dto(sensor_paths[[2]],
          skip = 20, col = col,
          date_format = "%d/%m/%Y %H:%M:%S",
          quiet = quiet
        )
      }
    )

    # Crop time
    if (!is.null(crop_start)) {
      tag$light <- subset(tag$light, date >= crop_start)
    }
    if (!is.null(crop_end)) {
      tag$light <- subset(tag$light, date < crop_end)
    }
  }

  # Read acceleration
  if (!is.na(sensor_paths[[3]])) {
    tag$acceleration <- switch(tools::file_ext(sensor_paths[[3]]),
      "acceleration" = {
        tag_create_dto(sensor_paths[[3]], col = 4, quiet = quiet)
      },
      "deg" = {
        # Check that it is a valid Migrate Technology file
        assertthat::assert_that(grepl("Migrate Technology", readLines(sensor_paths[[3]], n = 1)))
        line2 <- readLines(sensor_paths[[3]], n = 2)[[2]]
        v <- regmatches(line2, regexpr("Type: \\K\\d+", line2, perl = TRUE))
        if (v < 13) {
          cli::cli_abort(
            "The acceleration file {.file {sensor_paths[[3]]}} is not compatible. Line 2 should \\
             contains {.val Types:x}, with x>=13."
          )
        }

        # find column index with acceleration
        col <- which(utils::read.delim(sensor_paths[[3]],
          skip = 19, nrow = 1, header = FALSE, sep = ""
        ) == "Zact")
        if (!(col > 0)) {
          cli::cli_abort(
            "The light file {.file {sensor_paths[[3]]}} is not compatible. Line 20 \\
            should contains {.val Zact}"
          )
        }

        # Read file
        tag_create_dto(sensor_paths[[3]],
          skip = 20, col = col,
          date_format = "%d/%m/%Y %H:%M:%S",
          quiet = quiet
        )
      }
    )

    # Crop time
    if (!is.null(crop_start)) {
      tag$acceleration <- subset(tag$acceleration, date >= crop_start)
    }
    if (!is.null(crop_end)) {
      tag$acceleration <- subset(tag$acceleration, date < crop_end)
    }
  }

  # Add parameter information
  tag$param$pressure_file <- sensor_paths[[1]]
  tag$param$light_file <- sensor_paths[[2]]
  tag$param$acceleration_file <- sensor_paths[[3]]
  tag$param$crop_start <- crop_start
  tag$param$crop_end <- crop_end

  return(tag)
}

#' Read data file with a DTO format (Date Time Observation)
#'
#' @param sensor_path Full path of the file (directory + file)
#' @param skip Number of lines of the data file to skip before beginning to read data.
#' @param colIndex of the column of the data to take as observation.
#' @param date_format Format of the date (see [`strptime()`]).
#' @family tag
#' @noRd
tag_create_dto <- function(sensor_path, skip = 6, col = 3, date_format = "%d.%m.%Y %H:%M", quiet = FALSE) {
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
  if (!quiet) {
    cli::cli_inform(c("v" = "Read {sensor_path}\f"))
  }
  return(df)
}
