#' Create a `tag` object
#'
#' @description
#' Create a GeoPressureR `tag` object from sensor(s) data and optionally crops at specific
#' date.
#'
#' The `*_file` can be determined automatically with `"auto"`, provided as a full pathname of an
#' existing file or be matched with a file extension using a [regex] expression (e.g.,
#' `"*.pressure"` matches any file finishing with `pressure`).
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
#' @param id unique identifier of a tag.
#' @param directory path of the directory where the tag files can be read.
#' @param pressure_file name of the file with pressure data. Full pathname, `"auto"`, or finishing
#' with extensions `.pressure` or `.deg` (required).
#' @param light_file name of the file with light data. Full pathname, `"auto"`, `NULL` to ignore or
#'  extensions `*.glf`, `*.lux`.
#' @param acceleration_file name of the file with acceleration data. Full pathname, `"auto"`, `NULL`
#'  to ignore or extension `.acceleration`, `.deg` (or `NA` to ignore).
#' @param crop_start remove all data before this date (POSIXct or character in UTC).
#' @param crop_end remove all data after this date (POSIXct or character in UTC).
#' @param quiet logical to hide messages about the progress.
#'
#' @return a GeoPressureR `tag` object containing
#' - `param` parameter object (see [param_create])
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
#'   acceleration_file = NULL
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
                       pressure_file = "auto",
                       light_file = "auto",
                       acceleration_file = "auto",
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

  # Read Pressure
  pressure_path <- tag_create_detect(pressure_file, directory, c("*.pressure", "*.deg"))

  if (is.na(pressure_path)) {
    cli::cli_abort(c(
      "x" = "There are no pressure file {.val {names(sensor_paths[1])}}",
      "!" = "Pressure file are required"
    ))
  } else if (tools::file_ext(pressure_path) == "pressure") {
    tag$pressure <- tag_create_dto(pressure_path,
      crop_start = crop_start,
      crop_end = crop_end,
      quiet = quiet
    )
  } else if (tools::file_ext(pressure_path) == "deg") {
    # Check that it is a valid Migrate Technology file
    assertthat::assert_that(grepl("Migrate Technology", readLines(pressure_path, n = 1)))
    line2 <- readLines(pressure_path, n = 2)[[2]]
    v <- regmatches(line2, regexpr("Type: \\K\\d+", line2, perl = TRUE))
    if (v < 13) {
      cli::cli_abort(
        "The pressure file {.file {sensor_paths[1]}} is not compatible. Line 2 should \\
             contains {.val Types:x}, with x>=13."
      )
    }

    # find column index with pressure
    col <- which(utils::read.delim(pressure_path,
      skip = 19, nrow = 1, header = FALSE, sep = ""
    ) == "P(Pa)")
    if (!(col > 0)) {
      cli::cli_abort(
        "The pressure file {.file {pressure_path}} is not compatible. Line 20 \\
            should contains {.val P(Pa)}"
      )
    }

    # Read file
    pres <- tag_create_dto(pressure_path,
      skip = 20, col = col,
      date_format = "%d/%m/%Y %H:%M:%S",
      crop_start = crop_start,
      crop_end = crop_end,
      quiet = quiet
    )

    # convert Pa in hPa
    pres$value <- pres$value / 100
    tag$pressure <- pres
  } else {
    cli::cli_abort(
      "The pressure file {.val {pressure_path}} should have the extension {.val .pressure} \\
        or {.val .deg}"
    )
  }

  if (nrow(tag$pressure) == 0) {
    cli::cli_abort(c(
      "!" = "Empty {.field pressure} sensor dataset from {.file {pressure_path}}",
      ">" = "Check crop date."
    ))
  }


  # Read light
  light_path <- tag_create_detect(light_file, directory, c("*.glf", "*.lux"))
  if (!is.na(light_path)) {
    tag$light <- switch(tools::file_ext(light_path),
      "glf" = {
        tag_create_dto(light_path,
          crop_start = crop_start,
          crop_end = crop_end,
          quiet = quiet
        )
      },
      "lux" = {
        # find column index with light
        col <- which(utils::read.delim(
          light_path,
          skip = 19, nrow = 1, header = FALSE, sep = ""
        ) == "light(lux)")
        if (length(col)==0) {
          cli::cli_abort(
            "The light file {.file {light_path}} is not compatible. Line 20 \\
            should contains {.val light(lux)}"
          )
        }

        # Read file
        tag_create_dto(light_path,
          skip = 20, col = col,
          date_format = "%d/%m/%Y %H:%M:%S",
          crop_start = crop_start,
          crop_end = crop_end,
          quiet = quiet
        )
      }
    )
  }

  # Read acceleration
  acceleration_path <- tag_create_detect(acceleration_file, directory, c("*.acceleration", "*.deg"))
  if (!is.na(acceleration_path)) {
    tag$acceleration <- switch(tools::file_ext(acceleration_path),
      "acceleration" = {
        tag_create_dto(acceleration_path,
          col = 4,
          crop_start = crop_start,
          crop_end = crop_end,
          quiet = quiet
        )
      },
      "deg" = {
        # Check that it is a valid Migrate Technology file
        assertthat::assert_that(grepl("Migrate Technology", readLines(acceleration_path, n = 1)))
        line2 <- readLines(acceleration_path, n = 2)[[2]]
        v <- regmatches(line2, regexpr("Type: \\K\\d+", line2, perl = TRUE))
        if (v < 13) {
          cli::cli_abort(
            "The acceleration file {.file {acceleration_path}} is not compatible. Line 2 should \\
             contains {.val Types:x}, with x>=13."
          )
        }

        # find column index with acceleration
        col <- which(
          utils::read.delim(acceleration_path, skip = 19, nrow = 1, header = FALSE, sep = "" )
          == "Zact")
        if (length(col)==0) {
          cli::cli_abort(c(
            "x" = "The acceleration file {.file {acceleration_path}} is not compatible. Line 20 \\
            should contains {.val Zact}",
            ">" = "Make sure to chose the correct file or use {.val {NULL}} if you don't have an \\
            acceleration file"
          ))
        }

        # Read file
        tag_create_dto(acceleration_path,
          skip = 20, col = col,
          date_format = "%d/%m/%Y %H:%M:%S",
          crop_start = crop_start,
          crop_end = crop_end,
          quiet = quiet
        )
      }
    )
  }

  # Add parameter information
  tag$param$pressure_file <- pressure_path
  tag$param$light_file <- light_path
  tag$param$acceleration_file <- acceleration_path
  tag$param$crop_start <- crop_start
  tag$param$crop_end <- crop_end

  return(tag)
}

# Detect full path from the argument file.
#' @noRd
tag_create_detect <- function(file, directory, default = NULL) {
  if (is.na(file) | is.null(file)) {
    return(NA)
  }
  if (file.exists(file)) {
    return(file)
  }
  assertthat::assert_that(assertthat::is.dir(directory))

  if (file == "auto") {
    path <- list.files(directory,
      pattern = paste0(default, "$", collapse = "|"),
      full.names = TRUE
    )
  } else {
    path <- list.files(directory, pattern = glue::glue(file, "$"), full.names = TRUE)
  }

  if (length(path) == 0) {
    cli::cli_warn(c(
      "!" = glue::glue("No file is matching '", file, "'."),
      ">" = "This sensor will be ignored.\f"
    ))
    return(NA)
  }
  if (length(path) > 1) {
    cli::cli_warn(c(
      "!" = "Multiple files matching {.file {file}}: {path}",
      ">" = "The function will continue with the first one.\f"
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
#' @inheritParams tag_create
#' @family tag
#' @noRd
tag_create_dto <- function(sensor_path,
                           skip = 6,
                           col = 3,
                           date_format = "%d.%m.%Y %H:%M",
                           crop_start = NULL,
                           crop_end = NULL,
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
      x = "Invalid data in {.file {sensor_path)} at line(s): {20 + which(is.na(df$value))}",
      i = "Check and fix the corresponding lines"
    ))
  }

  # Crop time
  if (!is.null(crop_start)) {
    df <- df[df$date >= crop_start, ]
  }
  if (!is.null(crop_end)) {
    df <- df[df$date < crop_end, ]
  }

  if (length(unique(diff(df$date))) > 1) {
    dtime <- as.numeric(diff(df$date))
    cli::cli_warn("Irregular time spacing for {.file {sensor_path}}: \\
                  {df$date[which(dtime != dtime[1])]}.\f")
  }
  if (!quiet) {
    cli::cli_inform(c("v" = "Read {.file {sensor_path}}\f"))
  }
  return(df)
}
