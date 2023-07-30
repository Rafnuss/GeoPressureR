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
                          series = NULL,
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
  if (!is.null(series)) {
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
    series_name <- ifelse(is.null(series), "", glue::glue(" of ", series))
    cli::cli_warn(c(
      i = "The labelization file{series_name} is missing {missing_pres} timesteps and includes
      {nrow(csv) - nrow(df) + missing_pres} timestep which are not nedded. ",
      ">" = "We assumed no discard during the timestep missing."
    ))
  }

  return(df)
}
