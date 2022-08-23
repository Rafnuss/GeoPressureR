#' Read pam data
#'
#' Imports multi-sensor logger data from a folder and optionally crop at specific date. Read all
#' available file from the extension list provided.
#'
#' @param pathname path of the directory where the files are stored
#' @param extension list of file extensions to read (e.g., ".pressure", ".glf", ".gle",
#' ".acceleration", ".temperature" and ".magnetic")
#' @param crop_start Remove all date before this date
#' @param crop_end Remove all date after this date
#'
#' @return a list of data.frames of all measurements type from the extension list (see example)
#' @seealso [Vignette Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureR/articles/pressure-map.html)
#' @examples
#' pam_data <- pam_read(
#'   pathname = system.file("extdata", package = "GeoPressureR"),
#'   crop_start = "2017-06-20", crop_end = "2018-05-02"
#' )
#' summary(pam_data)
#' head(pam_data$id)
#' head(pam_data$acceleration)
#' head(pam_data$light)
#' head(pam_data$pressure)
#' head(pam_data$temperature)
#' @export
pam_read <- function(pathname,
                     extension = c("pressure", "glf", "acceleration", "temperature", "magnetic"),
                     crop_start = "1900-01-01",
                     crop_end = "2100-01-01") {
  stopifnot(dir.exists(pathname))
  stopifnot(is.character(extension))
  stopifnot(all(extension %in% c("pressure", "glf", "acceleration", "temperature", "magnetic")))

  # convert date to POSIXct date
  crop_start <- as.POSIXct(crop_start, tz = "UTC")
  crop_end <- as.POSIXct(crop_end, tz = "UTC")

  # find all files in the folder containing the extension
  files <- list.files(pathname, pattern = paste0(".*\\.(", paste(extension, collapse = "|"), ")$"))

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
    pam[[fname]] <- pam_read_file(paste0(pathname, "/", f), crop_start, crop_end)
  }

  return(pam)
}

#' Read pam file
#'
#' Read a specific pam file and return the corresponding data.frame.
#'
#' @param filename is the path where files are stored
#' @param crop_start posicxt object for date that pam data should start
#' @param crop_end posicxt object for date that pam data should end
#'
#' @seealso [`pam_read`]
#' @return a data.frame of the measurement
pam_read_file <- function(filename, crop_start, crop_end) {
  # read data as delimiter
  data_raw <- utils::read.delim(filename, skip = 6, sep = "", header = FALSE)

  # get and convert the date
  date <- as.POSIXct(strptime(paste(data_raw[, 1], data_raw[, 2]),
    tz = "UTC",
    format = "%d.%m.%Y %H:%M"
  ))

  # Filter date
  id_date <- date >= crop_start & date < crop_end

  # Create data.frame
  data <- data.frame(date = date[id_date])

  # Add other values
  if (grepl("acceleration", filename)) {
    data$obs <- data_raw[id_date, 4]
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

  return(data)
}









#' Automatic classification of pam
#'
#' This function uses activity data to classify migratory flapping flight. It returns the same data
#' list `pam` adding a column `ismig` to the data.frame `acceleration`. This fonction is inspired by
#' the function `classify_flap` from the [PAMLr package](https://github.com/KiranLDA/pamlr).
#'
#' @param pam data list
#' @param min_duration duration in minutes
#'
#' @return pam
#' @seealso [`pam_read()`], [flapping chapter of the PAMLr
#' manual](https://kiranlda.github.io/PAMLrManual/flapping.html), [Vignette Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureR/articles/pressure-map.html)
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
  stopifnot("obs" %in% names(pam$acceleration))
  stopifnot(is.numeric(min_duration))
  stopifnot(min_duration > 0)

  # Run a 2 class k mean clustering
  km <- stats::kmeans(pam$acceleration$obs[pam$acceleration$obs > 0], centers = 2)

  # classify all datapoints belonging to the high value cluster
  act_mig <- pam$acceleration$obs > mean(km$centers)

  # group continous activites (low or high) with and ID
  act_id <- c(1, cumsum(diff(as.numeric(act_mig)) != 0) + 1)

  # compute the time resolution of the datset
  dt <- as.double(pam$acceleration$date[2] - pam$acceleration$date[1], units = "mins")

  # Search all activity with high activity and with a duration above
  # min_duration
  tmp <- sapply(split(act_mig, act_id), unique) & table(act_id) * dt > min_duration

  # Classify acceleration accordingly
  pam$acceleration$ismig <- tmp[act_id]

  # plot(pam$acceleration$date[pam$acceleration$ismig],
  # pam$acceleration$obs[pam$acceleration$ismig])

  return(pam)
}




#' Compute stationary periods
#'
#' This function computes the stationary periods from classified acceleration data
#' (`pam$acceleration$ismig`).
#'
#' @param pam pam logger dataset list (see [`pam_read()`])
#' @return Same as input `pam` but with (1) a new data.frame of stationary periods `pam$sta` and (2)
#' a new column `sta_id` for pressure and light data.
#'
#' @examples
#' pam_data <- pam_read(
#'   pathname = system.file("extdata", package = "GeoPressureR"),
#'   crop_start = "2017-06-20", crop_end = "2018-05-02"
#' )
#' pam_data <- trainset_read(pam_data, pathname = system.file("extdata", package = "GeoPressureR"))
#' pam_data <- pam_sta(pam_data)
#' head(pam_data$pressure)
#' head(pam_data$light)
#' @seealso [`pam_read`], [`pam_classify`], [Vignette Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureR/articles/pressure-map.html)
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
  stopifnot("obs" %in% names(pam$acceleration))
  stopifnot("ismig" %in% names(pam$acceleration))
  stopifnot(is.data.frame(pam$light))
  stopifnot("date" %in% names(pam$light))
  stopifnot("obs" %in% names(pam$light))

  # Create a table of activities (migration or stationary)
  act_id <- c(1, cumsum(diff(as.numeric(pam$acceleration$ismig)) != 0) + 1)

  act <- data.frame(
    id = unique(act_id),
    start = do.call("c", lapply(split(pam$acceleration$date, act_id), min)),
    end = do.call("c", lapply(split(pam$acceleration$date, act_id), max)),
    mig = sapply(split(pam$acceleration$ismig, act_id), unique)
  )

  # filter to keep only migration activities
  act_mig <- act[act$mig, ]
  act_mig$duration <- act_mig$end - act_mig$start

  # construct stationary period table based on migration activity and pressure
  pam$sta <- data.frame(
    start = append(pam$acceleration$date[1], act_mig$end),
    end = append(act_mig$start, pam$acceleration$date[length(pam$acceleration$date)])
  )

  # Define ID for stationary period
  pam$sta$sta_id <- seq_len(nrow(pam$sta))

  # Assign to each pressure the stationary period to which it belong to.
  tmp <- mapply(function(start, end) {
    start < pam$pressure$date & pam$pressure$date < end
  }, pam$sta$start, pam$sta$end)
  tmp <- which(tmp, arr.ind = TRUE)
  pam$pressure$sta_id <- 0
  pam$pressure$sta_id[tmp[, 1]] <- tmp[, 2]

  # Assign to each light measurement the stationary period
  tmp <- mapply(function(start, end) {
    start < pam$light$date & pam$light$date < end
  }, pam$sta$start, pam$sta$end)
  tmp <- which(tmp, arr.ind = TRUE)
  pam$light$sta_id <- 0
  pam$light$sta_id[tmp[, 1]] <- tmp[, 2]

  return(pam)
}



#' Write classification of activity and pressure
#'
#' This function writes the csv file of the automatically labeled activity and pressure which can
#' be read with TRAINSET (https://trainset.geocene.com/).
#'
#' @param pam pam logger dataset list
#' @param pathname Path to the folder where the labeled files should be saved
#' @param filename Name for the file.
#'
#' @examples
#' \dontrun{
#' pam_data <- pam_read(
#'   pathname = system.file("extdata", package = "GeoPressureR"),
#'   crop_start = "2017-06-20", crop_end = "2018-05-02"
#' )
#' pam_data <- pam_classify(pam_data)
#' trainset_write(pam_data, pathname = system.file("extdata", package = "GeoPressureR"))
#' }
#' @seealso [`pam_read`], [`trainset_read()`], [Vignette Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureR/articles/pressure-map.html)
#' @export
trainset_write <- function(pam, pathname, filename = paste0(pam$id, "_act_pres")) {

  # Perform test
  stopifnot(is.list(pam))
  stopifnot("pressure" %in% names(pam))
  stopifnot(is.data.frame(pam$pressure))
  stopifnot("date" %in% names(pam$pressure))
  stopifnot("obs" %in% names(pam$pressure))
  stopifnot("acceleration" %in% names(pam))
  stopifnot(is.data.frame(pam$acceleration))
  stopifnot("date" %in% names(pam$acceleration))
  stopifnot("obs" %in% names(pam$acceleration))
  if (!("ismig" %in% names(pam$acceleration))) {
    pam$acceleration$ismig <- FALSE
  }
  if (!("isoutliar" %in% names(pam$pressure))) {
    pam$pressure$isoutliar <- FALSE
  }
  stopifnot(is.character(pathname))
  stopifnot(is.character(filename))
  # create path if does not exit
  if (!dir.exists(pathname)) {
    dir.create(pathname)
  }
  stopifnot(dir.exists(pathname))

  print(paste0(pathname, "/", filename, ".csv"))

  # write a combined data.frame of pressure and acceleration in csv.
  utils::write.csv(
    rbind(
      data.frame(
        series = "acceleration",
        timestamp = strftime(pam$acceleration$date, "%Y-%m-%dT%H:%M:%SZ",
          tz = "UTC"
        ),
        value = pam$acceleration$obs,
        label = ifelse(pam$acceleration$ismig, "1", "")
      ),
      data.frame(
        series = "pressure",
        timestamp = strftime(pam$pressure$date, "%Y-%m-%dT%H:%M:%SZ",
          tz = "UTC"
        ),
        value = pam$pressure$obs,
        label = ifelse(pam$pressure$isoutliar, "1", "")
      )
    ),
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
#' @return pam logger dataset list updated with the labels (`pam$pressure$isoutliar` and
#' `pam$acceleration$ismig`)
#'
#' @examples
#' pam_data <- pam_read(
#'   pathname = system.file("extdata", package = "GeoPressureR"),
#'   crop_start = "2017-06-20", crop_end = "2018-05-02"
#' )
#' pam_data <- trainset_read(pam_data, pathname = system.file("extdata", package = "GeoPressureR"))
#' head(pam_data$pressure)
#' head(pam_data$acceleration)
#' @seealso [`pam_read`], [Vignette Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureR/articles/pressure-map.html)
#' @export
trainset_read <- function(pam, pathname, filename = paste0(pam$id, "_act_pres-labeled.csv")) {

  # Perform test
  stopifnot(is.list(pam))
  stopifnot("pressure" %in% names(pam))
  stopifnot(is.data.frame(pam$pressure))
  stopifnot("date" %in% names(pam$pressure))
  stopifnot("obs" %in% names(pam$pressure))
  stopifnot("acceleration" %in% names(pam))
  stopifnot(is.list(pam$acceleration))
  stopifnot("date" %in% names(pam$acceleration))
  stopifnot("obs" %in% names(pam$acceleration))
  stopifnot(is.character(pathname))
  stopifnot(is.character(filename))
  stopifnot(dir.exists(pathname))
  fullpath <- paste0(pathname, "/", filename)
  stopifnot(file.exists(fullpath))

  # read the file
  csv <- utils::read.csv(fullpath)

  # check that the file is in the right format
  stopifnot(c("series", "timestamp", "label") %in% names(csv))

  csv$date <- strptime(csv$timestamp, "%FT%T", tz = "UTC")
  series <- NULL
  csv_acc <- subset(csv, series == "acceleration")
  csv_pres <- subset(csv, series == "pressure")

  id_acc_match <- match(as.numeric(pam$acceleration$date), as.numeric(csv_acc$date))
  pam$acceleration$ismig <- !is.na(csv_acc$label[id_acc_match])

  id_pres_match <- match(as.numeric(pam$pressure$date), as.numeric((csv_pres$date)))
  pam$pressure$isoutliar <- !is.na(csv_pres$label[id_pres_match])

  missing_acc <- sum(is.na(id_acc_match))
  missing_pres <- sum(is.na(id_pres_match))
  if (missing_acc > 0 || missing_pres > 0) {
    trainset_write(pam, pathname = tempdir(), filename = paste0(pam$id, "_act_pres-labeled"))
    warning(paste0(
      "The labelization file is missing ", missing_pres, " timesteps of pressure and ",
      missing_acc, " timesteps of acceleration and includes ",
      nrow(csv_pres) - nrow(pam$pressure) + missing_pres, " timestep of pressure and ",
      nrow(csv_acc) - nrow(pam$acceleration) + missing_acc, " timestep of acceleration",
      " which are not nedded. We assumed no migration and no outliar during the ",
      "timestep missing. You can find the updated labelization file at ", tempdir(), "/",
      pam$id, "_act_pres-labeled.csv"
    ))
  }

  return(pam)
}
