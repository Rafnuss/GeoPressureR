#' Read pam data
#'
#' Imports multi-sensor logger data from a folder and optionally crop at
#' specific date. Read all available file from the extension list AND which
#' exist in the folder.
#'
#' @param pathname path where files are stored
#' @param extension list of file extentions to read (e.g., ".pressure",
#' ".glf", ".gle",".acceleration", ".temperature" and ".magnetic")
#' @param crop_start is the date that pam data should start
#' @param crop_end is the date that pam data should end
#'
#' @return a list of all measurements
#'
#' @examples
#' pam_data <- pam_read(
#'   pathname = system.file("extdata", package = "GeoPressureR"),
#'   crop_start = "2017-06-20", crop_end = "2018-05-02"
#' )
#' summary(pam_data)
#' for (i in 1:length(pam_data)) {
#'   head(pam_data[[i]])
#' }
#' @export
pam_read <- function(pathname,
                     extension = c(
                       "pressure", "glf", "acceleration",
                       "temperature", "magnetic"
                     ),
                     crop_start = "1900-01-01",
                     crop_end = "2100-01-01") {
  stopifnot(dir.exists(pathname))
  stopifnot(is.character(extension))
  stopifnot(all(extension %in% c(
    "pressure", "glf", "acceleration",
    "temperature", "magnetic"
  )))

  # convert date to POSIXct date
  crop_start <- as.POSIXct(crop_start, tz = "UTC")
  crop_end <- as.POSIXct(crop_end, tz = "UTC")

  # find all files in the folder containing the extension
  files <- list.files(
    pathname,
    pattern = paste0(".*\\.(", paste(extension, collapse = "|"), ")$")
  )

  # Initialize the pam list
  pam <- list()

  # add identifier from the filename
  pam$id <- substr(files[1], 1, 4)

  # read each file
  for (f in files) {
    if (grepl("glf", f)) {
      fname <- "light"
    } else {
      fname <- strsplit(f, "\\.")[[1]][2]
    }
    pam[[fname]] <- pam_read_file(
      paste0(pathname, "/", f), crop_start, crop_end
    )
  }

  # return
  pam
}

#' Read pam file
#'
#' Read any pam file and return the corresponding data.frame.
#'
#' @param filename is the path where files are stored
#' @param crop_start posicxt object for date that pam data should start
#' @param crop_end posicxt object for date that pam data should end
#'
#' @return a data.frame of the measurement
#'
pam_read_file <- function(filename, crop_start, crop_end) {
  # read data as delimiter
  data_raw <- utils::read.delim(filename, skip = 6, sep = "", header = F)

  # get and convert the date
  date <- as.POSIXct(strptime(paste(data_raw[, 1], data_raw[, 2]),
    tz = "UTC", format = "%d.%m.%Y %H:%M"
  ))

  # Filter date
  id_date <- date >= crop_start & date < crop_end

  # Create data.frame
  data <- data.frame(
    date = date[id_date]
  )

  # Add other values
  if (grepl("acceleration", filename)) {
    data$pit <- data_raw[id_date, 3]
    data$act <- data_raw[id_date, 4]
  } else if (grepl("magnetic", filename)) {
    data$gx <- data_raw[id_date, 4]
    data$gy <- data_raw[id_date, 5]
    data$gz <- data_raw[id_date, 6]
    data$mx <- data_raw[id_date, 7]
    data$my <- data_raw[id_date, 8]
    data$mz <- data_raw[id_date, 9]
  } else {
    data$obs <- data_raw[id_date, 3]
  }

  # return
  data
}









#' Automatic classification of pam
#'
#' This function uses activity data to classify migratory flapping flight. It
#' returns the same dats list `pam` adding a column `class` to the data.frame
#' `acceleration`.
#'
#' This fonction is inspired by the function `classify_flap` from the
#' [pamLr package](https://github.com/KiranLDA/pamLr).
#'
#' @param pam data list
#' @param min_duration duration in minutes
#'
#' @return pam
#'
#' @examples
#' pam_data <- pam_read(
#'   pathname = system.file("extdata", package = "GeoPressureR"),
#'   crop_start = "2017-06-20", crop_end = "2018-05-02"
#' )
#' pam_data <- pam_classify(pam_data, min_duration = 30)
#' head(pam_data$acceleration)
#' @export
pam_classify <- function(pam,
                         min_duration = 30) {
  stopifnot(is.list(pam))
  stopifnot("acceleration" %in% names(pam))
  stopifnot(is.data.frame(pam$acceleration))
  stopifnot("date" %in% names(pam$acceleration))
  stopifnot("act" %in% names(pam$acceleration))
  stopifnot(is.numeric(min_duration))
  stopifnot(min_duration > 0)

  # Run a 2 class k mean clustering
  km <- stats::kmeans(pam$acceleration$act[pam$acceleration$act > 0],
    centers = 2
  )

  # classify all datapoints belonging to the high value cluster
  act_class <- pam$acceleration$act > mean(km$centers)

  # group continous activites (low or high) with and ID
  act_id <- c(1, cumsum(diff(as.numeric(act_class)) != 0) + 1)

  # compute the time resolution of the datset
  dt <- as.double(pam$acceleration$date[2] - pam$acceleration$date[1],
    units = "mins"
  )

  # Search all activity with high activity and with a duration above
  # min_duration
  tmp <- sapply(split(act_class, act_id), unique) &
    table(act_id) * dt > min_duration

  # Classify acceleration accordingly
  pam$acceleration$class <- tmp[act_id]

  # plot(pam$acceleration$date[pam$acceleration$class],
  # pam$acceleration$act[pam$acceleration$class])

  # return
  pam
}



#' Edit classification of activity and pressure
#'
#' This function perform three steps: (1) write the \code{csv} file of the
#' automatically labeled activity and pressure with
#' \code{\link{trainset_write}}, (2) open trainset in your broweser
#' (\url{https://trainset.geocene.com/}) so that you can edit
#' the labels and (3) read the exported \code{csv} file from trainset with
#' \code{\link{trainset_read}}.
#'
#' @param pam pam logger dataset list
#' @param pathname Path to the folder where the labeled files should be
#'   saved
#' @param filename Name for the file.
#' @return pam logger dataset list updated with the new label named \code{class}
#'   (\code{pam$pressure$class} and \code{pam$acceleration$class})
#' @export
trainset_edit <- function(pam,
                          pathname,
                          filename = paste0(pam$id, "_act_pres-labeled.csv")) {

  # Create file from pam if no labeled file for this bird exist
  if (!file.exists(paste0(pathname, filename))) {
    trainset_write(
      pam,
      pathname,
      filename
    )

    # Edit the file in browser
    utils::browseURL("https://trainset.geocene.com/")

    # When labilization finished and new file saved, press enter
    cond <- T
    while (cond) {
      readline(prompt = "Press [enter] to continue")
      if (!file.exists(paste0(pathname, filename, "-labeled.csv"))) {
        cond <- F
      } else {
        warning(
          paste0(
            "No labelized file found. Make sure you exported the file from
            trainset as ",
            paste0(pathname, filename, "-labeled.csv")
          )
        )
      }
    }
  }

  # Read the new file and return the updated pam data
  trainset_read(
    pam,
    pathname,
    filename
  )
}



#' Write classification of activity and pressure
#'
#' This function writes the csv file of the automatically labeled activity and
#' pressure which can be read with TRAINSET (https://trainset.geocene.com/).
#'
#' @param pam pam logger dataset list
#' @param pathname Path to the folder where the labeled files should be
#' saved
#' @param filename Name for the file.
#'
#' @examples
#' \dontrun{
#' pam_data <- pam_read(
#'   pathname = system.file("extdata", package = "GeoPressureR"),
#'   crop_start = "2017-06-20", crop_end = "2018-05-02"
#' )
#' pam_data <- pam_classify(pam_data)
#' trainset_write(pam_data,
#'   pathname = system.file("extdata", package = "GeoPressureR")
#' )
#' }
#' @export
trainset_write <- function(pam,
                           pathname,
                           filename = paste0(pam$id, "_act_pres")) {

  # Perform test
  stopifnot(is.list(pam))
  stopifnot("pressure" %in% names(pam))
  stopifnot(is.data.frame(pam$pressure))
  stopifnot("date" %in% names(pam$pressure))
  stopifnot("obs" %in% names(pam$pressure))
  stopifnot("acceleration" %in% names(pam))
  stopifnot(is.data.frame(pam$acceleration))
  stopifnot("date" %in% names(pam$acceleration))
  stopifnot("act" %in% names(pam$acceleration))
  stopifnot("class" %in% names(pam$acceleration))
  stopifnot(is.character(pathname))
  stopifnot(is.character(filename))
  # create path if does not exit
  if (!dir.exists(pathname)) {
    dir.create(pathname)
  }
  stopifnot(dir.exists(pathname))

  # write a combined data.frame of pressure and acceleration in csv.
  utils::write.csv(
    rbind(
      data.frame(
        series = "acceleration",
        timestamp = strftime(pam$acceleration$date, "%Y-%m-%dT%H:%M:%SZ",
          tz = "UTC"
        ),
        value = pam$acceleration$act,
        label = ifelse(pam$acceleration$class, "1", "")
      ),
      data.frame(
        series = "pressure",
        timestamp = strftime(pam$pressure$date, "%Y-%m-%dT%H:%M:%SZ",
          tz = "UTC"
        ),
        value = pam$pressure$obs,
        label = ""
      )
    ),
    paste0(pathname, filename, ".csv"),
    row.names = FALSE
  )

  # no return
}



#' Read classification of activity and pressure
#'
#' This function read an exported csv file from trainset
#' (https://trainset.geocene.com/) and update the pam logger dataset
#'
#' @param pam pam logger dataset list
#' @param pathname Path to the folder where the labeled file is.
#' @param filename Name of the file.
#' @return pam logger dataset list updated with the new label named `class`
#' (`pam$pressure$class` and `pam$acceleration$class`)
#'
#' @examples
#' pam_data <- pam_read(
#'   pathname = system.file("extdata", package = "GeoPressureR"),
#'   crop_start = "2017-06-20", crop_end = "2018-05-02"
#' )
#' pam_data <- trainset_read(pam_data,
#'   pathname = system.file("extdata", package = "GeoPressureR")
#' )
#' head(pam_data$pressure)
#' head(pam_data$acceleration)
#' @export
trainset_read <- function(pam,
                          pathname,
                          filename = paste0(pam$id, "_act_pres-labeled.csv")) {

  # Perform test
  stopifnot(is.list(pam))
  stopifnot("pressure" %in% names(pam))
  stopifnot(is.data.frame(pam$pressure))
  stopifnot("date" %in% names(pam$pressure))
  stopifnot("obs" %in% names(pam$pressure))
  stopifnot("acceleration" %in% names(pam))
  stopifnot(is.list(pam$acceleration))
  stopifnot("date" %in% names(pam$acceleration))
  stopifnot("act" %in% names(pam$acceleration))
  stopifnot(is.character(pathname))
  stopifnot(is.character(filename))
  stopifnot(dir.exists(pathname))
  fullpath <- paste0(pathname, "/", filename)
  stopifnot(file.exists(fullpath))

  # read the file
  csv <- utils::read.csv(fullpath)

  # check that the file is in the right format and same size as pam data
  stopifnot("series" %in% names(csv))
  stopifnot(
    length(csv$label) == length(pam$acceleration$date) + length(pam$pressure$date)
  )

  # assign label value to class
  pam$acceleration$class <- !is.na(csv$label[csv$series == "acceleration"])
  pam$pressure$class <- !is.na(csv$label[csv$series == "pressure"])

  # return
  pam
}



#' Compute stationary periods
#'
#' This function computes the table of stationary periods from the class of
#' acceleration `pam$acceleration$class` and add it to the pam data as `sta_id`
#'
#' @param pam pam logger dataset list
#' @return pam logger dataset list with a the dataframe of stationary periods
#' `pam$sta` as well as the new label named `sta_id` (`pam$pressure$sta_id` and
#' `pam$acceleration$sta_id`)
#'
#' @examples
#' pam_data <- pam_read(
#'   pathname = system.file("extdata", package = "GeoPressureR"),
#'   crop_start = "2017-06-20", crop_end = "2018-05-02"
#' )
#' pam_data <- trainset_read(pam_data,
#'   pathname = system.file("extdata", package = "GeoPressureR")
#' )
#' pam_data <- pam_sta(pam_data)
#' head(pam_data$pressure)
#' head(pam_data$acceleration)
#' @export
pam_sta <- function(pam) {

  # Perform test
  stopifnot(is.list(pam))
  stopifnot("pressure" %in% names(pam))
  stopifnot(is.data.frame(pam$pressure))
  stopifnot("date" %in% names(pam$pressure))
  stopifnot("obs" %in% names(pam$pressure))
  stopifnot("acceleration" %in% names(pam))
  stopifnot(is.data.frame(pam$acceleration))
  stopifnot("date" %in% names(pam$acceleration))
  stopifnot("act" %in% names(pam$acceleration))
  stopifnot("class" %in% names(pam$acceleration))
  stopifnot(is.data.frame(pam$light))
  stopifnot("date" %in% names(pam$light))
  stopifnot("obs" %in% names(pam$light))

  # Create a table of activities (migration or stationary)
  act_id <- c(1, cumsum(diff(as.numeric(pam$acceleration$class)) != 0) + 1)

  act <- data.frame(
    id = unique(act_id),
    start = do.call("c", lapply(split(pam$acceleration$date, act_id), min)),
    end = do.call("c", lapply(split(pam$acceleration$date, act_id), max)),
    mig = sapply(split(pam$acceleration$class, act_id), unique)
  )

  # filter to keep only migration activities
  act_mig <- act[act$mig, ]
  act_mig$duration <- act_mig$end - act_mig$start

  # construct stationary period table based on migration activity and pressure
  pam$sta <- data.frame(
    start = c(pam$pressure$date[1], act_mig$end),
    end = c(act_mig$start, utils::tail(pam$pressure$date, n = 1))
  )

  # Compute the duration
  pam$sta$duration <- pam$sta$end - pam$sta$start
  pam$sta$next_flight_duration <- c(act_mig$duration, 0)
  pam$sta$sta_id <- seq_len(nrow(pam$sta))

  # Assign to each pressure the stationary period to which it belong to.
  pressure_sta_id <- sapply(
    pam$pressure$date, function(x) which(pam$sta$start < x & x < pam$sta$end)
  )
  pressure_sta_id[sapply(pressure_sta_id, function(x) length(x) == 0)] <- 0
  pam$pressure$sta_id <- unlist(pressure_sta_id)

  # Assign to each light measurement the stationary period
  light_sta_id <- sapply(
    pam$light$date, function(x) which(pam$sta$start < x & x < pam$sta$end)
  )
  light_sta_id[sapply(light_sta_id, function(x) length(x) == 0)] <- 0
  pam$light$sta_id <- unlist(light_sta_id)

  # return the updated list
  pam
}
