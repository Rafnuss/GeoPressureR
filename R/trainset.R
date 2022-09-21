
#' Write classification of activity and pressure
#'
#' This function writes the csv file of the automatically labeled activity and pressure which can
#' be read with TRAINSET (https://trainset.geocene.com/).
#'
#' Optionally, it can also export a reference dataset for pressure `pam$pressure$ref` as another
#' series to be visualized on TRAINSET, but without impacting the labeling process.
#'
#' @param pam pam logger dataset list
#' @param pathname Path to the folder where the labeled files should be saved
#' @param filename Name for the file.
#'
#' @examples
#' pam <- pam_read(
#'   pathname = system.file(
#'     "extdata/0_PAM/18LX",
#'     package = "GeoPressureR"
#'   )
#' )
#' trainset_write(pam, pathname = system.file("extdata/1_pressure/labels/",
#'   package = "GeoPressureR"
#' ))
#' @seealso [`pam_read`], [`trainset_read()`], [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#edit-activity-on-trainset)
#' @export
trainset_write <- function(pam,
                           pathname = "data/1_pressure/labels/",
                           filename = paste0(pam$id, "_act_pres")) {

  # Perform test
  assertthat::assert_that(is.list(pam))
  assertthat::assert_that(assertthat::has_name(pam, c("pressure", "acceleration")))
  assertthat::assert_that(is.data.frame(pam$pressure))
  assertthat::assert_that(assertthat::has_name(pam$pressure, c("date", "obs")))
  assertthat::assert_that(is.data.frame(pam$acceleration))
  assertthat::assert_that(assertthat::has_name(pam$acceleration, c("date", "obs")))
  if (!assertthat::has_name(pam$acceleration, "ismig")) {
    pam$acceleration$ismig <- FALSE
  }
  if (!assertthat::has_name(pam$pressure, "isoutlier")) {
    if (assertthat::has_name(pam$pressure, "isoutliar")) {
      warning(
        "pam$pressure$isoutliar is deprecated in favor of pam$pressure$isoutlier. This code will ",
        "continue but update your code and data to be compatible with futur version of ",
        "GeoPressureR."
      )
      pam$pressure$isoutlier <- pam$pressure$isoutliar
    } else {
      pam$pressure$isoutlier <- FALSE
    }
  }
  assertthat::assert_that(is.character(pathname))
  assertthat::assert_that(is.character(filename))
  # create path if does not exit
  if (!dir.exists(pathname)) {
    dir.create(pathname)
  }
  assertthat::assert_that(dir.exists(pathname))

  # Combine the variable
  df <- rbind(
    data.frame(
      series = "acceleration",
      timestamp = strftime(pam$acceleration$date[!is.na(pam$acceleration$obs)],
        "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
      ),
      value = pam$acceleration$obs[!is.na(pam$acceleration$obs)],
      label = ifelse(pam$acceleration$ismig[!is.na(pam$acceleration$obs)], "1", "")
    ),
    data.frame(
      series = "pressure",
      timestamp = strftime(pam$pressure$date[!is.na(pam$pressure$obs)], "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
      ),
      value = pam$pressure$obs[!is.na(pam$pressure$obs)],
      label = ifelse(pam$pressure$isoutlier[!is.na(pam$pressure$obs)], "1", "")
    )
  )

  # Add optional reference dataset
  if (assertthat::has_name(pam$pressure, "obs_ref")) {
    df <- rbind(
      df,
      data.frame(
        series = "pressure_reference",
        timestamp = strftime(pam$pressure$date[!is.na(pam$pressure$obs_ref)], "%Y-%m-%dT%H:%M:%SZ",
          tz = "UTC"
        ),
        value = pam$pressure$obs_ref[!is.na(pam$pressure$obs_ref)],
        label = ""
      )
    )
  }

  # write a combined data.frame of pressure and acceleration in csv.
  utils::write.csv(
    df,
    paste0(pathname, "/", filename, ".csv"),
    row.names = FALSE
  )
}



#' Read classification of activity and pressure
#'
#' This function read an exported csv file from trainset (https://trainset.geocene.com/) and update
#' the pam logger dataset
#'
#' @param pam pam logger dataset list
#' @param pathname Path to the folder where the labeled file is.
#' @param filename Name of the file.
#' @return pam logger dataset list updated with the labels (`pam$pressure$isoutlier` and
#' `pam$acceleration$ismig`)
#'
#' @examples
#' pam <- pam_read(pathname = system.file("extdata/0_PAM/18LX", package = "GeoPressureR"))
#' pam <- pam_classify(pam)
#' pam <- trainset_read(pam, pathname = system.file("extdata/1_pressure/labels/",
#'   package = "GeoPressureR"
#' ))
#' head(pam$pressure)
#' head(pam$acceleration)
#' @seealso [`pam_read`], [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#edit-activity-on-trainset)
#' @export
trainset_read <- function(pam,
                          pathname = "data/1_pressure/labels/",
                          filename = paste0(pam$id, "_act_pres-labeled.csv")) {

  # Perform test
  assertthat::assert_that(is.list(pam))
  assertthat::assert_that(assertthat::has_name(pam, c("pressure", "acceleration")))
  assertthat::assert_that(is.data.frame(pam$pressure))
  assertthat::assert_that(assertthat::has_name(pam$pressure, c("date", "obs")))
  assertthat::assert_that(is.data.frame(pam$acceleration))
  assertthat::assert_that(assertthat::has_name(pam$acceleration, c("date", "obs")))
  assertthat::assert_that(is.character(pathname))
  assertthat::assert_that(is.character(filename))
  assertthat::assert_that(dir.exists(pathname))
  fullpath <- paste0(pathname, "/", filename)
  assertthat::assert_that(file.exists(fullpath))

  # read the file
  csv <- utils::read.csv(fullpath)

  # check that the file is in the right format
  assertthat::assert_that(assertthat::has_name(csv, "series"))
  assertthat::assert_that(assertthat::has_name(csv, "timestamp"))
  assertthat::assert_that(assertthat::has_name(csv, "label"))

  csv$date <- strptime(csv$timestamp, "%FT%T", tz = "UTC")
  series <- NULL
  csv_acc <- subset(csv, series == "acceleration")
  csv_pres <- subset(csv, series == "pressure")

  id_acc_match <- match(as.numeric(pam$acceleration$date), as.numeric(csv_acc$date))
  pam$acceleration$ismig <- !is.na(csv_acc$label[id_acc_match])

  id_pres_match <- match(as.numeric(pam$pressure$date), as.numeric((csv_pres$date)))
  pam$pressure$isoutlier <- !is.na(csv_pres$label[id_pres_match])

  missing_acc <- sum(is.na(id_acc_match))
  missing_pres <- sum(is.na(id_pres_match))
  if (missing_acc > 0 || missing_pres > 0) {
    trainset_write(pam, pathname = tempdir(), filename = paste0(pam$id, "_act_pres-labeled"))
    warning(paste0(
      "The labelization file is missing ", missing_pres, " timesteps of pressure and ",
      missing_acc, " timesteps of acceleration and includes ",
      nrow(csv_pres) - nrow(pam$pressure) + missing_pres, " timestep of pressure and ",
      nrow(csv_acc) - nrow(pam$acceleration) + missing_acc, " timestep of acceleration",
      " which are not nedded. We assumed no migration and no outlier during the ",
      "timestep missing. You can find the updated labelization file at ", tempdir(), "/",
      pam$id, "_act_pres-labeled.csv"
    ))
  }

  return(pam)
}
