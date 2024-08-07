#' Create a `tag` object
#'
#' @description
#' Create a GeoPressureR `tag` object from the data collected by a tracking device. This data needs
#' to include at least pressure and optionally a light and/or acceleration.
#'
#' The current implementation can read files from the following three manufacturer:
#' - [Swiss Ornithological Institute (`soi`)](https://bit.ly/3QI6tkk)
#'    - `pressure_file = "*.pressure"`
#'    - `light_file = "*.glf"` (optional)
#'    - `acceleration_file = "*.acceleration"` (optional)
#' - [Migrate Technology (`migratetech`)](http://www.migratetech.co.uk/):
#'    - `pressure_file = "*.deg"`
#'    - `light_file = "*.lux"` (optional)
#'    - `acceleration_file = "*.deg"` (optional)
#' - [Lund CAnMove (`lund`)](https://bit.ly/3P6quyi)
#'    - `pressure_file = "*_press.xlsx"`
#'    - `light_file = "*_acc.xlsx"` (optional)
#'    - `acceleration_file = "*_acc.xlsx"` (optional)
#'
#'  You can also enter the data manually (`manufacturer = "manual"`) by providing the data.frame to
#'  `pressure_file`:
#'    - `pressure_file`: data.frame with column date and value.
#'    - `light_file`: (optional) data.frame with column date and value.
#'    - `acceleration_file`: (optional) data.frame with column date and value.
#'
#' By default `manufacturer = NULL`, the manufacturer is determined automatically from the content
#' of the `directory`. You can also specify manually the file with a full pathname or the file
#' extension using a [regex] expression (e.g., `"*.pressure"` matches any file ending with
#' `pressure`).
#'
#' Please create [an issue on Github](https://github.com/Rafnuss/GeoPressureR/issues/new) if you
#' have data in a format that is not yet supported.
#'
#' This function can be used to crop the data at specific date, for instance to remove pre-equipment
#' or post-retrieval data.
#'
#' @param id unique identifier of a tag.
#' @param manufacturer One of `NULL`, `"soi"`, `"migratetech"`, `"lund"` or `"manual"`
#' @param directory path of the directory where the tag files can be read.
#' @param pressure_file name of the file with pressure data. Full pathname  or finishing
#' with extensions (e.g., `"*.pressure"`, `"*.deg"` or `"*_press.xlsx"`).
#' @param light_file name of the file with light data. Full pathname  or finishing
#' with extensions (e.g., `"*.glf"`, `"*.lux"` or `"*_acc.xlsx"`).
#' @param acceleration_file name of the file with acceleration data. Full pathname  or finishing
#' with extensions (e.g., `"*.acceleration"`, `"*.deg"` or `"*_acc.xlsx"`).
#' @param temperature_file name of the file with temperature data. Full pathname  or finishing
#' with extensions (e.g., `"*.temperature"`, `"*.deg"`).
#' @param crop_start remove all data before this date (POSIXct or character in UTC).
#' @param crop_end remove all data after this date (POSIXct or character in UTC).
#' @param quiet logical to hide messages about the progress.
#'
#' @return a GeoPressureR `tag` object containing
#' - `param` parameter object (see [param_create])
#' - `pressure` data.frame with columns for `date` and `value`
#' - `light` (optional) same structure as pressure
#' - `acceleration` (optional) same structure as pressure
#'
#' @examples
#' setwd(system.file("extdata", package = "GeoPressureR"))
#'
#' # Read all sensor file
#' tag <- tag_create("18LX")
#'
#' print(tag)
#'
#' # Read only pressure and crop date
#' tag <- tag_create("18LX",
#'   light_file = NULL,
#'   acceleration_file = NULL,
#'   crop_start = "2017-08-01",
#'   crop_end = "2017-08-05"
#' )
#'
#' print(tag)
#'
#' # You can also specify the exact file in case multiple files with the
#' # same extension exist in your directory (migratetech data)
#' tag <- tag_create("CB621",
#'   pressure_file = "CB621_BAR.deg",
#'   light_file = "CB621.lux",
#'   acceleration_file = NULL
#' )
#'
#' print(tag)
#'
#' # You can specify the data manually with
#' pressure <- data.frame(
#'   date = as.POSIXct(c(
#'     "2017-06-20 00:00:00 UTC", "2017-06-20 01:00:00 UTC",
#'     "2017-06-20 02:00:00 UTC", "2017-06-20 03:00:00 UTC"
#'   ), tz = "UTC"),
#'   value = c(1000, 1000, 1000, 1000)
#' )
#' tag_create(id = "xxx", pressure_file = pressure)
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
                       temperature_file = NULL,
                       quiet = FALSE) {
  assertthat::assert_that(is.character(id))
  assertthat::assert_that(is.logical(quiet))

  if (is.null(manufacturer)) {
    if (is.data.frame(pressure_file)) {
      manufacturer <- "manual"
    } else {
      assertthat::assert_that(assertthat::is.dir(directory))
      if (any(grepl("\\.pressure$", list.files(directory)))) {
        manufacturer <- "soi"
      } else if (any(grepl("\\.deg$", list.files(directory)))) {
        manufacturer <- "migratetech"
      } else if (any(grepl("_press\\.xlsx$", list.files(directory)))) {
        manufacturer <- "lund"
      } else {
        cli::cli_abort(c(
          "x" = "We were not able to determine the {.var manufacturer} of tag from the directory \\
        {.field {directory}}",
          ">" = "Check that this directory contains the file with pressure data (i.e., with \\
        extension {.val .pressure}, {.val .deg} or {.val _press.xlsx})"
        ))
      }
    }
  }
  assertthat::assert_that(is.character(manufacturer))
  manufacturer_possible <- c("auto", "soi", "migratetech", "lund", "manual")
  if (!any(manufacturer %in% manufacturer_possible)) {
    cli::cli_abort(c(
      "x" = "{.var manufacturer} needs to be one of {.val {manufacturer_possible}}"
    ))
  }

  # Create tag
  tag <- structure(list(
    param = param_create(id = id)
  ), class = "tag")

  if (manufacturer == "soi") {
    tag <- tag_create_soi(
      tag,
      directory = directory,
      pressure_file = pressure_file,
      light_file = light_file,
      acceleration_file = acceleration_file,
      temperature_file = temperature_file,
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
  } else if (manufacturer == "lund") {
    tag <- tag_create_lund(
      tag,
      directory = directory,
      pressure_file = pressure_file,
      acceleration_light_file = acceleration_file,
      quiet = quiet
    )
  } else if (manufacturer == "manual") {
    tag <- tag_create_manual(
      tag,
      directory = directory,
      pressure_file = pressure_file,
      light_file = light_file,
      acceleration_file = acceleration_file,
      temperature_file = temperature_file,
      quiet = quiet
    )
  }

  ## Crop date
  tag <- tag_create_crop(tag, crop_start = crop_start, crop_end = crop_end)
  if (nrow(tag$pressure) == 0) {
    cli::cli_abort(c(
      "!" = "Empty {.field pressure} sensor dataset",
      ">" = "Check crop date."
    ))
  }

  # Add parameter information
  tag$param$manufacturer <- manufacturer
  tag$param$crop_start <- crop_start
  tag$param$crop_end <- crop_end
  tag$param$directory <- directory

  return(tag)
}



# Read Swiss Ornithological Institutue (SOI) tag files
#' @noRd
tag_create_soi <- function(tag,
                           directory,
                           pressure_file = NULL,
                           light_file = NULL,
                           acceleration_file = NULL,
                           temperature_file = NULL,
                           quiet) {
  # Read Pressure
  if (is.null(pressure_file)) {
    pressure_file <- "*.pressure"
  }
  pressure_path <- tag_create_detect(pressure_file, directory, quiet = quiet)
  if (is.null(pressure_path)) {
    cli::cli_abort(c(
      "x" = "There are no pressure file {.val {pressure_path}}",
      "!" = "Pressure file are required!"
    ))
  }
  tag$pressure <- tag_create_dto(pressure_path, quiet = quiet)

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
  if (is.null(light_file)) {
    acceleration_path <- tag_create_detect("*.acceleration", directory, quiet = TRUE)
  } else {
    acceleration_path <- tag_create_detect(acceleration_file, directory, quiet = quiet)
  }
  if (!is.null(acceleration_path)) {
    tag$acceleration <- tag_create_dto(acceleration_path, col = 4, quiet = quiet)
  }

  # Read temperature
  if (is.null(temperature_file)) {
    temperature_path <- tag_create_detect("*.temperature", directory, quiet = TRUE)
  } else {
    temperature_path <- tag_create_detect(temperature_file, directory, quiet = quiet)
  }
  if (!is.null(temperature_path)) {
    tag$temperature <- tag_create_dto(temperature_path, col = 3, quiet = quiet)
  }

  # Add parameter information
  tag$param$pressure_file <- pressure_path
  tag$param$light_file <- light_path
  tag$param$acceleration_file <- acceleration_path
  tag$param$temperature_file <- temperature_path

  setting_path <- tag_create_detect("*.settings", directory, quiet = TRUE)
  if (!is.null(setting_path)) {
    tag$param$soi_settings <- jsonlite::fromJSON(setting_path)
  }

  return(tag)
}



# Read Migrate Technology tag files
#' @noRd
tag_create_migratetech <- function(tag,
                                   directory,
                                   deg_file,
                                   light_file = NULL,
                                   quiet) {
  # Read Pressure
  if (is.null(deg_file)) {
    deg_file <- "*.deg"
  }
  deg_path <- tag_create_detect(deg_file, directory, quiet = quiet)
  if (is.null(deg_path)) {
    cli::cli_abort(c(
      "x" = "There are no pressure file {.val {deg_file}}",
      "!" = "Pressure file are required!"
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
    tag$param$acceleration_file <- deg_path
  }

  # Read temperature
  col <- which(hdr == "T('C)")
  if (length(col) != 0) {
    tag$temperature <- tag_create_dto(deg_path,
      skip = 20, col = col,
      date_format = "%d/%m/%Y %H:%M:%S",
      quiet = quiet
    )
    tag$param$temperature_file <- deg_path
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
  tag$param$pressure_file <- deg_path
  tag$param$light_file <- light_path

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
      "x" = "There are no pressure file {.val {pressure_path}}",
      "!" = "Pressure file are required"
    ))
  }
  xls <- readxl::read_excel(pressure_path, .name_repair = "unique_quiet")
  tag$pressure <- data.frame(
    date = as.POSIXct(xls$`Date & time`, tz = "UTC"),
    value = xls$`Pressure [hPa]`
  )
  if (!quiet) {
    cli::cli_inform(c("v" = "Read {.file {pressure_path}}\f"))
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
      cli::cli_inform(c("v" = "Read {.file {acc_light_path}}\f"))
    }
  }

  # Add parameter information
  tag$param$pressure_file <- pressure_path
  tag$param$light_file <- acc_light_path
  tag$param$acceleration_file <- acc_light_path

  return(tag)
}



# Read Migrate Technology tag files
#' @noRd
tag_create_manual <- function(tag,
                              directory,
                              pressure_file,
                              light_file = NULL,
                              acceleration_file = NULL,
                              temperature_file = NULL,
                              quiet) {
  # Read Pressure
  assertthat::assert_that(is.data.frame(pressure_file))
  assertthat::assert_that(assertthat::has_name(pressure_file, c("date", "value")))
  assertthat::assert_that(inherits(pressure_file$date, "POSIXct"))
  assertthat::assert_that(assertthat::are_equal(attr(pressure_file$date, "tzone"), "UTC"))
  if (min(pressure_file$value, na.rm = TRUE) < 250 ||
    1100 < max(pressure_file$value, na.rm = TRUE)) {
    cli::cli_warn("Pressure observation should be between 250 hPa (~10000m) and 1100 hPa \\
  (sea level at 1013hPa). Check unit of pressure data.frame provided.\f")
  }
  tag$pressure <- pressure_file

  # Read light
  if (!is.null(light_file)) {
    assertthat::assert_that(assertthat::has_name(light_file, c("date", "value")))
    assertthat::assert_that(inherits(light_file$date, "POSIXct"))
    assertthat::assert_that(assertthat::are_equal(attr(light_file$date, "tzone"), "UTC"))
    tag$light <- light_file
  }

  # Read acceleration
  if (!is.null(acceleration_file)) {
    assertthat::assert_that(assertthat::has_name(acceleration_file, c("date", "value")))
    assertthat::assert_that(inherits(acceleration_file$date, "POSIXct"))
    assertthat::assert_that(assertthat::are_equal(attr(acceleration_file$date, "tzone"), "UTC"))
    tag$acceleration <- acceleration_file
  }

  # Read acceleration
  if (!is.null(temperature_file)) {
    assertthat::assert_that(assertthat::has_name(temperature_file, c("date", "value")))
    assertthat::assert_that(inherits(temperature_file$date, "POSIXct"))
    assertthat::assert_that(assertthat::are_equal(attr(temperature_file$date, "tzone"), "UTC"))
    tag$temperature <- temperature_file
  }

  # Add parameter information
  tag$param$pressure_file <- "manual"
  tag$param$light_file <- "manual"
  tag$param$acceleration_file <- "manual"
  tag$param$temperature_file <- "manual"

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

  path <- list.files(directory,
    pattern = glue::glue(file, "$"),
    full.names = TRUE
  )

  path <- path[!grepl("~\\$", path)]

  if (length(path) == 0) {
    if (!quiet) {
      cli::cli_warn(c(
        "!" = glue::glue("No file is matching '", file, "'."),
        ">" = "This sensor will be ignored.\f"
      ))
    }
    return(NULL)
  }
  if (length(path) > 1) {
    cli::cli_warn(c(
      "!" = "Multiple files matching {.var {file}}: {.file {path}}",
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
      x = "Invalid data in {.file {sensor_path)} at line(s): {20 + which(is.na(df$value))}",
      i = "Check and fix the corresponding lines"
    ))
  }

  if (!quiet) {
    cli::cli_inform(c("v" = "Read {.file {sensor_path}}\f"))
  }
  return(df)
}

#' Crop sensor data.frame
#' @noRd
tag_create_crop <- function(tag,
                            crop_start,
                            crop_end) {
  for (sensor in c("pressure", "light", "acceleration", "temperature")) {
    if (sensor %in% names(tag)) {
      # Crop time
      if (!is.null(crop_start)) {
        tag[[sensor]] <- tag[[sensor]][tag[[sensor]]$date >= as.POSIXct(crop_start), ]
      }
      if (!is.null(crop_end)) {
        tag[[sensor]] <- tag[[sensor]][tag[[sensor]]$date < as.POSIXct(crop_end), ]
      }

      # Check irregular time
      if (length(unique(diff(tag[[sensor]]$date))) > 1) {
        # nolint start
        dtime <- as.numeric(diff(tag[[sensor]]$date))
        cli::cli_warn("Irregular time spacing for {.field {sensor}}: \\
                  {tag[[sensor]]$date[which(dtime != dtime[1])]}.\f")
        # nolint end
      }
    }
  }
  return(tag)
}
