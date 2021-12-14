#' Edit classification of activity and pressure
#'
#' This function perform three steps: (1) write the csv file of the
#' automatically labeled activity and pressure with `trainset.write`, (2) open
#' trainset in your broweser (https://trainset.geocene.com/) so that you can
#' edit the labels and (3) read the exported csv file from trainset with
#' `trainset.read`.
#'
#' @param PAM_data PAM logger dataset list (see https://github.com/KiranLDA/PAMLr for details)
#' @param file.location Path to the folder where the labeled files should be saved
#' @param file.name Name for the file.
#' @return PAM logger dataset list updated with the new label named `class` (`PAM_data$pressure$class` and `PAM_data$acceleration$class`)
#' @export
trainset.edit <- function(PAM_data,
                          file.location,
                          file.name = paste0(PAM_data$id, '_act_pres')) {

  # Create file from PAM_data if no labeled file for this bird exist
  if (!file.exists(paste0(file.location, file.name, "-labeled.csv"))) {
    trainset.write(PAM_data,
                   file.location,
                   file.name)

    # Edit the file in browser
    browseURL("https://trainset.geocene.com/")

    # When labilization finished and new file saved, press enter
    cond = T
    while (cond) {
      readline(prompt = "Press [enter] to continue")
      if (!file.exists(paste0(file.location, file.name, "-labeled.csv"))) {
        cond = F
      } else {
        warning(
          paste0(
            "No labelized file found. Make sure you exported the file from trainset as ",
            paste0(file.location, file.name, "-labeled.csv")
          )
        )
      }
    }
  }

  # Read the new file and return the updated PAM data
  trainset.read(PAM_data,
                file.location,
                file.name)
}



#' Write classification of activity and pressure
#'
#' This function writes the csv file of the automatically labeled activity and pressure which can be read with TRAINSET (https://trainset.geocene.com/).
#'
#' @param PAM_data PAM logger dataset list (see https://github.com/KiranLDA/PAMLr for details)
#' @param file.location Path to the folder where the labeled files should be saved
#' @param file.name Name for the file.
#' @export
trainset.write <- function(PAM_data,
                           file.location,
                           file.name = paste0(PAM_data$id, '_act_pres')) {

  # Perform test
  testthat::expect_type(PAM_data, "list")
  testthat::expect_true("pressure" %in% names(PAM_data))
  testthat::expect_type(PAM_data$pressure, "list")
  testthat::expect_true("date" %in% names(PAM_data$pressure))
  testthat::expect_true("obs" %in% names(PAM_data$pressure))
  testthat::expect_true("acceleration" %in% names(PAM_data))
  testthat::expect_type(PAM_data$acceleration, "list")
  testthat::expect_true("date" %in% names(PAM_data$acceleration))
  testthat::expect_true("act" %in% names(PAM_data$acceleration))
  testthat::expect_true(dir.exists(file.location))
  testthat::expect_type(file.location, "character")
  testthat::expect_type(file.name, "character")
  # create path if does not exit
  if(!dir.exists(file.location)) {
    dir.create(file.location)
  }
  testthat::expect_true(dir.exists(file.location))

  # write a combined data.frame of pressure and acceleration in csv.
  write.csv(
    rbind(
      data.frame(
        series = "acceleration",
        timestamp = strftime(PAM_data$acceleration$date , "%Y-%m-%dT%H:%M:%S%z"),
        value = PAM_data$acceleration$act,
        label = PAM_data$acceleration$class
      ),
      data.frame(
        series = "pressure",
        timestamp = strftime(PAM_data$pressure$date , "%Y-%m-%dT%H:%M:%S%z"),
        value = PAM_data$pressure$obs,
        label = ""
      )
    ),
    paste0(file.location, file.name, ".csv"),
    row.names = FALSE
  )

  # no return
}



#' Read classification of activity and pressure
#'
#' This function read an exported csv file from trainset (https://trainset.geocene.com/) and update the PAM logger dataset
#'
#' @param PAM_data PAM logger dataset list (see https://github.com/KiranLDA/PAMLr for details)
#' @param file.location Path to the folder where the labeled file is.
#' @param file.name Name of the file.
#' @return PAM logger dataset list updated with the new label named `class` (`PAM_data$pressure$class` and `PAM_data$acceleration$class`)
#' @export
trainset.read <- function(PAM_data,
                          file.location,
                          file.name = paste0(PAM_data$id, '_act_pres')) {

  # Perform test
  testthat::expect_type(PAM_data, "list")
  testthat::expect_true("pressure" %in% names(PAM_data))
  testthat::expect_type(PAM_data$pressure, "list")
  testthat::expect_true("date" %in% names(PAM_data$pressure))
  testthat::expect_true("obs" %in% names(PAM_data$pressure))
  testthat::expect_true("acceleration" %in% names(PAM_data))
  testthat::expect_type(PAM_data$acceleration, "list")
  testthat::expect_true("date" %in% names(PAM_data$acceleration))
  testthat::expect_true("act" %in% names(PAM_data$acceleration))
  testthat::expect_true(dir.exists(file.location))
  testthat::expect_type(file.location, "character")
  testthat::expect_type(file.name, "character")
  # create path if does not exit
  if(!dir.exists(file.location)) {
    dir.create(file.location)
  }
  testthat::expect_true(dir.exists(file.location))

  # read the file
  tmp = read.csv(paste0(file.location, file.name, "-labeled.csv"))

  # check that the file is in the right format and same size as PAM data
  testthat::expect_true("series" %in% names(tmp))
  testthat::expect_length(tmp$label, length(PAM_data$acceleration$date)+length(PAM_data$pressure$date))

  # Assign label value to class
  PAM_data$acceleration$class = tmp$label[tmp$series == "acceleration"]
  PAM_data$pressure$class = tmp$label[tmp$series == "pressure"]

  # return the updated list
  PAM_data
}
