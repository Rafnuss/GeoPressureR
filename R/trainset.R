#' Write classification of activity and pressure
#'
#' This function writes the csv file of the automatically labeled activity and pressure which can
#' be read with TRAINSET (https://trainset.geocene.com/).
#'
#' Optionally, it can also export a reference dataset for pressure `tag$pressure$ref` as another
#' series to be visualized on TRAINSET, but without impacting the labeling process.
#'
#' @param tag data logger dataset list
#' @param pathname Path to the folder where the labeled files should be saved
#' @param filename Name for the file.
#'
#' @examples
#' tag <- tag_read(
#'   pathname = system.file(
#'     "extdata/0_tag/18LX",
#'     package = "GeoPressureR"
#'   )
#' )
#' trainset_write(tag, pathname = system.file("extdata/1_pressure/labels/",
#'   package = "GeoPressureR"
#' ))
#' @seealso [`tag_read`], [`trainset_read()`], [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#edit-activity-on-trainset)
#' @export
trainset_write <- function(tag,
                           pathname = "data/1_pressure/labels/",
                           filename = paste0(tag$id, "_act_pres.csv")) {
  assertthat::assert_that(is.list(tag))
  assertthat::assert_that(assertthat::has_name(tag, "pressure"))
  assertthat::assert_that(is.data.frame(tag$pressure))
  assertthat::assert_that(assertthat::has_name(tag$pressure, c("date", "value")))
  assertthat::assert_that(is.character(pathname))
  assertthat::assert_that(is.character(filename))

  if (!assertthat::has_name(tag$acceleration, "ismig")) {
    tag$acceleration$ismig <- FALSE
  }
  if (!assertthat::has_name(tag$pressure, "isoutlier")) {
    tag$pressure$isoutlier <- FALSE
  }
  # create path if does not exit
  if (!dir.exists(pathname)) {
    dir.create(pathname)
  }
  assertthat::assert_that(dir.exists(pathname))

  # Combine the variable
  df <- rbind(
    data.frame(
      series = "acceleration",
      timestamp = strftime(tag$acceleration$date[!is.na(tag$acceleration$value)],
        "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
      ),
      value = tag$acceleration$value[!is.na(tag$acceleration$value)],
      label = ifelse(tag$acceleration$ismig[!is.na(tag$acceleration$value)], "1", "")
    ),
    data.frame(
      series = "pressure",
      timestamp = strftime(tag$pressure$date[!is.na(tag$pressure$value)], "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
      ),
      value = tag$pressure$value[!is.na(tag$pressure$value)],
      label = ifelse(tag$pressure$isoutlier[!is.na(tag$pressure$value)], "1", "")
    )
  )

  # Add optional reference dataset
  if (assertthat::has_name(tag$pressure, "value_ref")) {
    df <- rbind(
      df,
      data.frame(
        series = "pressure_reference",
        timestamp = strftime(tag$pressure$date[!is.na(tag$pressure$value_ref)],
                             "%Y-%m-%dT%H:%M:%SZ",
          tz = "UTC"
        ),
        value = tag$pressure$value_ref[!is.na(tag$pressure$value_ref)],
        label = ""
      )
    )
  }

  # write a combined data.frame of pressure and acceleration in csv.
  utils::write.csv(
    df,
    paste0(pathname, "/", filename),
    row.names = FALSE
  )
}



#' Read classification of activity and pressure
#'
#' This function read an exported csv file from [trainset](https://trainset.geocene.com/) and update
#' the data logger dataset
#'
#' @param tag data logger dataset list
#' @param pathname Path to the folder where the labeled file is.
#' @param filename Name of the file.
#' @return data logger dataset list updated with the labels (`tag$pressure$isoutlier` and
#' `tag$acceleration$ismig`)
#'
#' @examples
#' tag <- tag_read(pathname = system.file("extdata/0_tag/18LX", package = "GeoPressureR"))
#' tag <- tag_classify(tag)
#' tag <- trainset_read(tag, pathname = system.file("extdata/1_pressure/labels/",
#'   package = "GeoPressureR"
#' ))
#' head(tag$pressure)
#' head(tag$acceleration)
#' @seealso [`tag_read`], [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#edit-activity-on-trainset)
#' @export
trainset_read <- function(tag,
                          pathname = "data/1_pressure/labels/",
                          filename = paste0(tag$id, "_act_pres-labeled.csv")) {
  assertthat::assert_that(is.list(tag))
  assertthat::assert_that(assertthat::has_name(tag, "pressure"))
  assertthat::assert_that(is.data.frame(tag$pressure))
  assertthat::assert_that(assertthat::has_name(tag$pressure, c("date", "value")))
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

  # Check that all label are correct
  assertthat::assert_that(all(csv$label %in% c("flight", "discard", "") |
    startsWith(csv$label, "elev_")))

  # Convert to date format
  csv$date <- strptime(csv$timestamp, "%FT%T", tz = "UTC")

  # Extract pressure label
  csv_pres <- csv[csv$series == "pressure", ]
  id_pres_match <- match(as.numeric(tag$pressure$date), as.numeric((csv_pres$date)))
  tag$pressure$label <- !is.na(csv_pres$label[id_pres_match])
  missing_pres <- sum(is.na(id_pres_match))

  # Extract acceleration label
  if (any(csv$series == "acceleration")) {
    csv_acc <- csv[csv$series == "acceleration", ]
    id_acc_match <- match(as.numeric(tag$acceleration$date), as.numeric(csv_acc$date))
    tag$acceleration$ismig <- !is.na(csv_acc$label[id_acc_match])
    missing_acc <- sum(is.na(id_acc_match))
  }

  # Message in case missing label
  if (any(csv$series == "acceleration")) {
    if (missing_acc > 0 || missing_pres > 0) {
      trainset_write(tag, pathname = tempdir(), filename = paste0(tag$id, "_act_pres-labeled"))
      warning(paste0(
        "The labelization file is missing ", missing_pres, " timesteps of pressure and ",
        missing_acc, " timesteps of acceleration and includes ",
        nrow(csv_pres) - nrow(tag$pressure) + missing_pres, " timestep of pressure and ",
        nrow(csv_acc) - nrow(tag$acceleration) + missing_acc, " timestep of acceleration",
        " which are not nedded. We assumed no migration and no outlier during the ",
        "timestep missing. You can find the updated labelization file at ", tempdir(), "/",
        tag$id, "_act_pres-labeled.csv"
      ))
    }
  } else {
    if (missing_pres > 0) {
      trainset_write(tag, pathname = tempdir(), filename = paste0(tag$id, "_act_pres-labeled"))
      warning(paste0(
        "The labelization file is missing ", missing_pres, " timesteps of pressure and includes ",
        nrow(csv_pres) - nrow(tag$pressure) + missing_pres, " timestep of pressure",
        " which are not nedded. We assumed no migration and no outlier during the ",
        "timestep missing. You can find the updated labelization file at ", tempdir(), "/",
        tag$id, "_act_pres-labeled.csv"
      ))
    }
  }
}
