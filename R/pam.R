#' Read pam data
#'
#' Imports multi-sensor logger data from a folder (`pathname`) and optionally crop at specific date.
#' The `*_file` arguments are matched using a regex expression (e.g., `"*.pressure"` matches any
#' files with the extension `pressure`).
#'
#' Create [an issue on github](https://github.com/Rafnuss/GeoPressureR/issues/new) if you have data
#' in a format not supported yet.
#'
#' @param pathname path of the directory where the files are stored
#' @param pressure_file file with pressure data. Extension must be `.pressure`, `.deg`.
#' @param light_file file with light data. Extension must be `.glf`, `.lux` or `NA` if absent.
#' @param acceleration_file file with acceleration data. Extension must be `.acceleration`  or `NA`
#' if absent.
#' @param crop_start Remove all date before this date
#' @param crop_end Remove all date after this date
#' @param id Unique identifier of the track. Default (`NA`) is to take the part of
#' `pressure_file` up to a character `_` (e.g. `18LX` for `18LX_20180725.pressure`).
#'
#' @return a list of data.frames of pressure, light and acceleration.
#' @seealso [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#read-geolocator-data)
#' @examples
#' pam <- pam_read(
#'   pathname = system.file("extdata/0_PAM/18LX", package = "GeoPressureR"),
#'   crop_start = "2017-06-20", crop_end = "2018-05-02"
#' )
#' summary(pam)
#' head(pam$id)
#' head(pam$pressure)
#' head(pam$light)
#' head(pam$acceleration)
#' @export
pam_read <- function(pathname,
                     pressure_file = "*.pressure",
                     light_file = "*.glf",
                     acceleration_file = "*.acceleration",
                     crop_start = "1900-01-01",
                     crop_end = "2100-01-01",
                     id = NA) {
  stopifnot(dir.exists(pathname))

  # convert date to POSIXct date and check format
  crop_start <- as.POSIXct(crop_start, tz = "UTC")
  crop_end <- as.POSIXct(crop_end, tz = "UTC")

  # Check existence of the file
  pressure_path <- ifelse(is.na(pressure_file), "", pam_read_check(pathname, pressure_file))
  light_path <- ifelse(is.na(light_file), "", pam_read_check(pathname, light_file))
  acceleration_path <- ifelse(is.na(acceleration_file), "",
    pam_read_check(pathname, acceleration_file)
  )

  if (is.na(id)) {
    id <- strsplit(basename(pressure_path), "_")[[1]][1]
  } else if (id == "basename") {
    id <- basename(pathname)
  }

  # Initialize the PAM list
  pam <- list(
    pressure = switch(tools::file_ext(pressure_path),
      "pressure" = {
        subset(pam_read_delim_dto(pressure_path), date >= crop_start & date < crop_end)
      },
      "deg" = {
        subset(
          pam_read_delim_dto(pressure_path, skip = 20, col = 4, date_format = "%d/%m/%Y %H:%M:%S"),
          date >= crop_start & date < crop_end
        )
      },
      {
        data.frame()
      }
    ),
    light = switch(tools::file_ext(light_path),
      "glf" = {
        subset(pam_read_delim_dto(light_path), date >= crop_start & date < crop_end)
      },
      "lux" = {
        subset(
          pam_read_delim_dto(pressure_path, skip = 20, col = 3, date_format = "%d/%m/%Y %H:%M:%S"),
          date >= crop_start & date < crop_end
        )
      },
      {
        data.frame()
      }
    ),
    acceleration = switch(tools::file_ext(acceleration_path),
      "acceleration" = {
        subset(pam_read_delim_dto(acceleration_path), date >= crop_start & date < crop_end)
      },
      {
        data.frame()
      }
    ),
    id = id
  )

  return(pam)
}

#' Check that a given `pathname` and `file` exists and is unique.
#'
#' @param pathname is the path where file is
#' @param file is the name of he file
#' @seealso [`pam_read()`]
pam_read_check <- function(pathname, file) {
  path <- list.files(pathname, pattern = paste0(file, "$"), full.names = TRUE)
  if (length(path) == 0) {
    warning(paste0("No file is matching '", file, "'. This file will be ignored."))
    path <- ""
  } else if (length(path) > 1) {
    warning(paste0(
      "Multiple files matching '", file, "': \n", paste(path, collapse = "\n"),
      ". \nThe function will continue with the first one."
    ))
    path <- path[1]
  }
  return(path)
}

#' Read data file with a DTO format (Date Time Observation)
#'
#' @param path is the full path of the file (pathname + filename)
#' @param skip is the number of lines of the data file to skip before beginning to read data.
#' @param col is the the index of the column of the data to take as observation
#' @param date_format format of the date
#' @seealso [`pam_read()`]
pam_read_delim_dto <- function(path, skip = 6, col = 3, date_format = "%d.%m.%Y %H:%M") {
  data_raw <- utils::read.delim(path, skip = skip, sep = "", header = FALSE)
  data.frame(
    date = as.POSIXct(strptime(paste(data_raw[, 1], data_raw[, 2]),
      tz = "UTC",
      format = date_format
    )),
    obs = data_raw[, col]
  )
}









#' Automatic classification of pam
#'
#' This function uses activity data to classify migratory flapping flight. It returns the same data
#' list `pam` adding a column `ismig` to the data.frame `acceleration`. This fonction is inspired by
#' the function `classify_flap` from the [PAMLr package](https://github.com/KiranLDA/pamlr).
#'
#' @param pam logger dataset list. See [`pam_read()`].
#' @param min_duration duration in minutes
#'
#' @return pam
#' @seealso [`pam_read()`], [flapping chapter of the PAMLr
#' manual](https://kiranlda.github.io/PAMLrManual/flapping.html), [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#automatic-classification-of-activity)
#' @examples
#' pam <- pam_read(
#'   pathname = system.file("extdata/0_PAM/18LX", package = "GeoPressureR"),
#'   crop_start = "2017-06-20", crop_end = "2018-05-02"
#' )
#' pam <- pam_classify(pam, min_duration = 30)
#' head(pam$acceleration)
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
#' @param pam PAM logger dataset list. See [`pam_read()`].
#' @return Same as input `pam` but with (1) a new data.frame of stationary periods `pam$sta` and (2)
#' a new column `sta_id` for pressure and light data.
#'
#' @examples
#' pam <- pam_read(
#'   pathname = system.file("extdata/0_PAM/18LX", package = "GeoPressureR"),
#'   crop_start = "2017-06-20", crop_end = "2018-05-02"
#' )
#' pam <- trainset_read(pam, pathname = system.file("extdata/1_pressure/labels",
#' package = "GeoPressureR"))
#' pam <- pam_sta(pam)
#' head(pam$pressure)
#' head(pam$light)
#' @seealso [`pam_read`], [`pam_classify`], [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#identify-stationary-periods)
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
    sta_id = seq_len(nrow(act_mig) + 1),
    start = append(pam$acceleration$date[1], act_mig$end),
    end = append(act_mig$start, pam$acceleration$date[length(pam$acceleration$date)])
  )

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
#' pam <- pam_read(
#'   pathname = system.file(
#'     "extdata/0_PAM/18LX",
#'     package = "GeoPressureR"
#'   ),
#'   crop_start = "2017-06-20", crop_end = "2018-05-02"
#' )
#' pam <- pam_classify(pam)
#' trainset_write(pam, pathname = system.file("extdata/1_pressure/labels/",
#' package = "GeoPressureR"))
#' }
#' @seealso [`pam_read`], [`trainset_read()`], [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#edit-activity-on-trainset)
#' @export
trainset_write <- function(pam,
                           pathname = "data/1_pressure/labels/",
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
#' pam <- pam_read(pathname = system.file("extdata/0_PAM/18LX", package = "GeoPressureR"))
#' pam <- pam_classify(pam)
#' pam <- trainset_read(pam, pathname = system.file("extdata/1_pressure/labels/",
#' package = "GeoPressureR"))
#' head(pam$pressure)
#' head(pam$acceleration)
#' @seealso [`pam_read`], [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#edit-activity-on-trainset)
#' @export
trainset_read <- function(pam,
                          pathname = "data/1_pressure/labels/",
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
