# Read Swiss Ornithological Institute (SOI) tag files
#' @noRd
tag_create_soi <- function(id,
                           directory = glue::glue("./data/raw-tag/{id}"),
                           pressure_file = NULL,
                           light_file = NULL,
                           acceleration_file = NULL,
                           temperature_external_file = NULL,
                           temperature_internal_file = NULL,
                           magnetic_file = NULL,
                           quiet = FALSE) {
  assertthat::assert_that(is.character(id))
  assertthat::assert_that(is.logical(quiet))

  # Create tag
  tag <- structure(list(param = param_create(id = id)), class = "tag")

  # Read SOI settings
  tryCatch(
    {
      setting_path <- tag_create_detect("*.settings", directory, quiet = TRUE)
      if (!is.null(setting_path)) {
        tag$param$soi_settings <- jsonlite::fromJSON(setting_path)

        if (tag$param$soi_settings$RecordingStop != "timestamp invalid") {
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
            {format_minutes(tag$param$drift)} which seems suspicious.",
              ">" = "Check for error (e.g. timezone)"
            ))
          }
        } else {
          tag$param$drift <- NULL
        }
      }
    },
    error = function(e) {
      cli::cli_alert_warning("Failed to load the SOI setting file {.file {setting_path}}.")
      message(e$message)
    }
  )

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
  if (!is.null(magnetic_path) && "soi_settings" %in% names(tag$param)) {
    tag$magnetic <- tag_create_soi_mag(magnetic_path,
      hw_version = tag$param$soi_settings,
      quiet = quiet
    )
    # Add information
    tag$param$mag_axis <- attr(tag$magnetic, "mag_axis")
    attr(tag$magnetic, "mag_axis") <- NULL
  }

  # Add parameter information
  tag$param$tag_create$pressure_file <- pressure_path
  tag$param$tag_create$light_file <- light_path
  tag$param$tag_create$acceleration_file <- acceleration_path
  tag$param$tag_create$temperature_external_file <- temperature_external_path
  tag$param$tag_create$temperature_internal_file <- temperature_internal_path
  tag$param$tag_create$magnetic_file <- magnetic_path
  tag$param$tag_create$manufacturer <- "soi"
  tag$param$tag_create$directory <- directory

  return(tag)
}

#' Correct magnetic data from SOI data
#'
#' @description
#' The function applies the following three operations:
#'
#' 1. Correction magnetic value in Gauss (https://en.wikipedia.org/wiki/Gauss_(unit))
#' 2. Correction gravity value in G (https://en.wikipedia.org/wiki/Gravitational_constant)
#' 3. Compute mag and acc in the standard forward-right-down
#'
#' There are two sensors compatible:
#' - LSM303D: hardware version v2.3 and lower
#' - LSM303AGR: hardware version 2.4 and higher
#'
#' Version is detected automatically from `tag$param$soi_settings`
#' @noRd
tag_create_soi_mag <- function(magnetic_path,
                               hw_version,
                               quiet = FALSE) {
  assertthat::assert_that(is.logical(quiet))
  assertthat::assert_that(file.exists(magnetic_path))

  mag <- tag_create_dto(magnetic_path, col = seq(4, 9), quiet = quiet)
  names(mag) <- c("date", "mX", "mY", "mZ", "gX", "gY", "gZ")

  # Detect hardware version
  if (is.list(hw_version)) {
    key <- intersect(c("HW Version", "HWVersion"), names(hw_version))
    if (length(key) > 0) {
      hw_version <- hw_version[[key[1]]]
      hw_version <- sub("^(hw_)?GDL3pam[-_]?v?[-_]?", "", hw_version, ignore.case = TRUE)
      hw_version <- as.double(hw_version)
    }
  }
  if (!is.numeric(hw_version) || length(hw_version) != 1) {
    if (!quiet) {
      cli::cli_alert_warning(
        c(
          "!" = "The hardware version {.val {hw_version}} could not be detected.",
          ">" = "The magnetic data is not corrected and {.code mag} contains the raw data."
        )
      )
    }
    return(mag)
  }

  # Automatically define correction values
  if (hw_version <= 2.3) { # LSM303D
    corr_m <- 0.00016 # unit G. raw value stored of the ADC (analog-digital converter) to Gauss
    corr_g <- 4 / 65536 # 16-Bit resolution on a full scale of 4g (+/-2g)
    mag_axis <- c("left", "backward", "down")
  } else { # LSM303AGR
    corr_m <- 0.0015 # value stored to Gauss
    corr_g <- 4 / 4096 # 12-Bit resolution on a full scale of 4g (+/-2g)
    mag_axis <- c("backward", "left", "down")
  }

  # Apply correction
  mag[c("mX", "mY", "mZ")] <- mag[c("mX", "mY", "mZ")] * corr_m
  mag[c("gX", "gY", "gZ")] <- mag[c("gX", "gY", "gZ")] * corr_g

  # Convert the axes orientation to have the format forward-right-down
  mag <- mag_axes(mag, mag_axis)

  # Name variables
  map <- c(
    gX = "acceleration_x", gY = "acceleration_y", gZ = "acceleration_z",
    mX = "magnetic_x", mY = "magnetic_y", mZ = "magnetic_z"
  )
  names(mag) <- ifelse(names(mag) %in% names(map), map[names(mag)], names(mag))

  return(mag)
}


#' Convert the mag axes to forward-right-down
#' @noRd
mag_axes <- function(mag, mag_axis = "auto") {
  if (all(mag_axis == "auto")) {
    xyz <- c(stats::median(mag$gX), stats::median(mag$gY), stats::median(mag$gZ))

    mag_axis <- c("", "", "")

    # We assume that the z axis is on the 3rd coordinate an that gravity is going down =)
    mag_axis[3] <- ifelse(xyz[3] > 0, "down", "up")

    # The large value between 1st and 2nd coordinate defines the sway (forward-backward) and we
    # assume that roll is positive
    if (abs(xyz[1]) > abs(xyz[2])) {
      if (xyz[1] > 0) {
        mag_axis[1] <- "backward"
        mag_axis[2] <- ifelse(mag_axis[3] == "up", "right", "left")
      } else {
        mag_axis[1] <- "forward"
        mag_axis[2] <- ifelse(mag_axis[3] == "up", "left", "right")
      }
    } else {
      if (xyz[2] > 0) {
        mag_axis[2] <- "backward"
        mag_axis[1] <- ifelse(mag_axis[3] == "up", "left", "right")
      } else {
        mag_axis[2] <- "forward"
        mag_axis[1] <- ifelse(mag_axis[3] == "up", "right", "left")
      }
    }
  }

  # We use a forward-right-down system. Which means that the sensor needs to be oriented
  ref_axis_name <- matrix(c("forward", "backward", "right", "left", "down", "up"), 2, 3)

  # Assert that all axis are provided correctly, and on of each direction
  assertthat::assert_that(all(mag_axis %in% ref_axis_name))
  assertthat::assert_that(any(ref_axis_name[, 1] %in% mag_axis))
  assertthat::assert_that(any(ref_axis_name[, 2] %in% mag_axis))
  assertthat::assert_that(any(ref_axis_name[, 3] %in% mag_axis))

  # Compute the index of the converted axes
  axes_ind <- ceiling(match(mag_axis, ref_axis_name) / 2)

  # Find the sign conversion for each of these axes
  axes_sign <- ifelse((match(mag_axis[axes_ind], ref_axis_name)) %% 2, 1, -1)

  # Extract the field to convert
  mag_g <- data.frame(mag$gX, mag$gY, mag$gZ)
  mag_g <- data.frame(mapply(`*`, mag_g[, axes_ind], axes_sign))
  mag$gX <- mag_g[, 1]
  mag$gY <- mag_g[, 2]
  mag$gZ <- mag_g[, 3]

  mag_m <- data.frame(mag$mX, mag$mY, mag$mZ)
  mag_m <- data.frame(mapply(`*`, mag_m[, axes_ind], axes_sign))
  mag$mX <- mag_m[, 1]
  mag$mY <- mag_m[, 2]
  mag$mZ <- mag_m[, 3]

  attr(mag, "mag_axis") <- mag_axis
  return(mag)
}
