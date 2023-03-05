#' Write data.frame into a csv file compatible with TRAINSET
#'
#' This function writes the csv file which can be open with TRAINSET
#' (https://trainset.geocene.com/). See https://trainset.geocene.com/help for details.
#'
#' @param df Data.frame written in csv. Columns name defined by the parameters below.
#' @param file Name of the label file to be saved.
#' @param series Character or vector to be used as name for series.
#' @param timestamp Column name of `df` to be used as timestamp.
#' @param value Column name of `df` to be used as value.
#' @param label Column name of `df` to be used as label. If column doesn't exist, use empty
#' characters.
#' @seealso [`tag_label_write()`]
#' @noRd
trainset_write <- function(df,
                           file,
                           series = "series",
                           timestamp = "date",
                           value = "value",
                           label = "label") {
  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(assertthat::has_name(df, c(timestamp, value)))
  assertthat::assert_that(is.character(file))

  # Create empty label if label doesn't exit
  if (!assertthat::has_name(df, label)) {
    df[[label]] <- ""
  }

  # Remove row with empty element
  df <- df[!is.na(df[[value]]), ]

  # Combine the variable
  df_trainset <- data.frame(
    series = df[[series]],
    timestamp = strftime(df[[timestamp]], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    value = df[[value]],
    label = df[[label]]
  )

  # Check if folder exist
  dir_file <- dirname(file)
  if (!assertthat::is.dir(dir_file)) {
    cli::cli_alert_warning("The directory {.file {dir_file}} does not exists.")
    res <- utils::askYesNo("Do you want to create it?")
    if (res) {
      dir.create(dir_file)
    } else {
      return(FALSE)
    }
  }

  # write a combined data.frame of pressure and acceleration in csv.
  utils::write.csv(
    df_trainset,
    file = file,
    row.names = FALSE
  )

  cli::cli_alert_success("{.file {file}} written successfully.")
  return(file)
}


#' Read trainset csv file and update a data.frame
#'
#' This function read an exported csv file from [trainset](https://trainset.geocene.com/) and update
#' the data logger dataset `tag`.
#'
#' @param df Data.frame on which to add/overwrite label column
#' @param file Name of the label file.
#' @param series Character to be used as series. If column doesn't exist, use empty
#' @param label Column name of `df` to be used as label.
#' @return Same data logger list as input, updated with the labels `label`
#'
#' @noRd
trainset_read <- function(df,
                          file,
                          series = NA,
                          timestamp = "date",
                          label = "label") {
  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(is.character(timestamp))
  assertthat::assert_that(is.character(label))
  assertthat::assert_that(assertthat::has_name(df, timestamp))
  assertthat::assert_that(is.character(file))
  assertthat::assert_that(file.exists(file))

  # read the file
  csv <- utils::read.csv(file)

  # check that the file is in the right format
  assertthat::assert_that(assertthat::has_name(csv, "series"))
  assertthat::assert_that(assertthat::has_name(csv, "timestamp"))
  assertthat::assert_that(assertthat::has_name(csv, "label"))

  # Convert to date format
  csv$date <- strptime(csv$timestamp, "%FT%T", tz = "UTC")

  # Extract only data from the corresponding series
  if (!is.na(series)) {
    csv <- csv[csv$series == series, ]
  }

  if (nrow(csv) == 0) {
    cli::cli_warn(c(
      i = "The labelization file does not contains label for {series}. ",
      ">" = "No labels are added to {series}."
    ))
    return(df)
  }

  # Erase existing labels
  df[[label]] <- ""

  # Find the corresponding time
  id_match <- match(as.numeric(df[[timestamp]]), as.numeric((csv$date)))

  # use label only if not NA (missing, see below for warning message)
  df[[label]][!is.na(id_match)] <- csv$label[id_match[!is.na(id_match)]]

  # Check for missing data
  missing_pres <- sum(is.na(id_match))

  if (missing_pres > 0) {
    series_name <- ifelse(is.na(series), "", glue::glue(" of ", series))
    cli::cli_warn(c(
      i = "The labelization file{series_name} is missing {missing_pres} timesteps and includes
      {nrow(csv) - nrow(df) + missing_pres} timestep which are not nedded. ",
      ">" = "We assumed no discard during the timestep missing."
    ))
  }

  return(df)
}
