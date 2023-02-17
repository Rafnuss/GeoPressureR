#' Write classification of activity and pressure
#'
#' This function writes the csv file of the automatically labeled activity and pressure which can
#' be read with TRAINSET (https://trainset.geocene.com/).
#'
#' Optionally, it can also export a reference dataset for pressure `tag$pressure$ref` as another
#' series to be visualized on TRAINSET, but without impacting the labeling process.
#'
#' @param tag Data logger list (see [`tag_read()`]).
#' @param directory Directory to the folder where the label file should be saved.
#' @param filename Name of the label file to be saved.
#'
#' @examples
#' tag <- tag_read(directory = system.file("extdata/0_tag/18LX", package = "GeoPressureR"))
#' tag <- trainset_write(
#'   tag,
#'   directory = system.file("extdata/1_pressure/labels/", package = "GeoPressureR")
#' )
#' @seealso [`tag_read()`], [`trainset_read()`], [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#edit-activity-on-trainset)
#' @export
trainset_write <- function(tag,
                           directory = "data/1_pressure/labels/",
                           filename = paste0(tag$id, "_act_pres.csv")) {
  assertthat::assert_that(is.list(tag))
  assertthat::assert_that(assertthat::has_name(tag, "pressure"))
  assertthat::assert_that(is.data.frame(tag$pressure))
  assertthat::assert_that(assertthat::has_name(tag$pressure, c("date", "value")))

  common_column <- c("date", "value", "label", "series")

  # Create empty label if it doesn't exit
  if (!assertthat::has_name(tag$pressure, "label")) {
    tag$pressure$label <- ""
  }

  # Add series name
  tag$pressure$series <- "pressure"
  df <- tag$pressure[common_column]

  # Add acceleration label if available
  if (assertthat::has_name(tag, "acceleration")) {
    if (!assertthat::has_name(tag$acceleration, "label")) {
      tag$acceleration$label <- ""
    }
    tag$acceleration$series <- "acceleration"
    df <- rbind(df[common_column], tag$acceleration[common_column])
  }

  # Add optional reference dataset
  if (assertthat::has_name(tag$pressure, "value_ref")) {
    tag$pressure$value <- tag$pressure$value_ref
    tag$pressure$series <- "pressure_ref"
    df <- rbind(df[common_column], tag$pressure[common_column])
  }

  # write a combined data.frame of pressure and acceleration in csv.
  full_path <- trainset_write_df(
    df,
    directory = directory,
    filename = filename
  )
  return (full_path)
}






#' Read classification of activity and pressure
#'
#' This function read an exported csv file from [trainset](https://trainset.geocene.com/) and update
#' the data logger dataset `tag`.
#'
#' @param tag Data logger dataset list (see [`tag_read()`]).
#' @param directory Directory where the label file can be found.
#' @param filename Name of the label file.
#' @return Same data logger list as input, updated with the labels `tag$pressure$label` and
#' optionally `tag$acceleration$label`.
#'
#' @examples
#' tag <- tag_read(directory = system.file("extdata/0_tag/18LX", package = "GeoPressureR"))
#' tag <- tag_classify(tag)
#' tag <- trainset_read(
#'   tag,
#'   directory = system.file("extdata/1_pressure/labels/", package = "GeoPressureR")
#' )
#' head(tag$pressure)
#' head(tag$acceleration)
#' @seealso [`tag_read()`], [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#edit-activity-on-trainset)
#' @export
trainset_read <- function(tag,
                          directory = "data/1_pressure/labels/",
                          filename = paste0(tag$id, "_act_pres-labeled.csv")) {
  assertthat::assert_that(is.list(tag))
  assertthat::assert_that(assertthat::has_name(tag, "pressure"))
  assertthat::assert_that(is.data.frame(tag$pressure))
  assertthat::assert_that(assertthat::has_name(tag$pressure, c("date", "value")))


  tag$pressure <- trainset_read_df(tag$pressure,
    directory = directory,
    filename = filename,
    series = "pressure"
  )

  # Check that all label are correct
  assertthat::assert_that(all(tag$pressure$label %in% c("flight", "discard", "") |
    startsWith(tag$pressure$label, "elev_")))

  # Extract acceleration label
  if (assertthat::has_name(tag, "acceleration")) {
    assertthat::assert_that(is.data.frame(tag$acceleration))
    assertthat::assert_that(assertthat::has_name(tag$acceleration, c("date", "value")))

    tag$acceleration <- trainset_read_df(tag$acceleration,
      directory = directory,
      filename = filename,
      series = "acceleration"
    )

    # Check that all label are correct
    assertthat::assert_that(all(tag$acceleration$label %in% c("flight", "discard", "") |
      startsWith(tag$acceleration$label, "elev_")))
  }

  return(tag)
}






#' Write data.frame into a csv file compatible with TRAINSET
#'
#' This function writes the csv file which can be open with TRAINSET
#' (https://trainset.geocene.com/). See https://trainset.geocene.com/help for details.
#'
#' @param df Data.frame written in csv. Columns name defined by the parameters below.
#' @param directory Directory to the folder where the label file should be saved.
#' @param filename Name of the label file to be saved.
#' @param series Character or vector to be used as name for series.
#' @param timestamp Column name of `df` to be used as timestamp.
#' @param value Column name of `df` to be used as value.
#' @param label Column name of `df` to be used as label. If column doesn't exist, use empty
#' characters.
#' @seealso [`trainset_write()`]
#' @export
trainset_write_df <- function(df,
                              directory,
                              filename,
                              series = "series",
                              timestamp = "date",
                              value = "value",
                              label = "label") {
  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(assertthat::has_name(df, c(timestamp, value)))
  assertthat::assert_that(is.character(directory))
  assertthat::assert_that(is.character(filename))

  # create path if does not exit
  if (!dir.exists(directory)) {
    dir.create(directory)
  }
  assertthat::assert_that(dir.exists(directory))

  full_path <- file.path(directory, filename)

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

  # write a combined data.frame of pressure and acceleration in csv.
  utils::write.csv(
    df_trainset,
    file = full_path,
    row.names = FALSE
  )

  return (full_path)
}






#' Read trainset csv file and update a data.frame
#'
#' This function read an exported csv file from [trainset](https://trainset.geocene.com/) and update
#' the data logger dataset `tag`.
#'
#' @param df Data.frame on which to add/overwrite label column
#' @param directory Directory where the label file can be found.
#' @param filename Name of the label file.
#' @param series Character to be used as series. If column doesn't exist, use empty
#' @param label Column name of `df` to be used as label.
#' @return Same data logger list as input, updated with the labels `label`
#'
#' @seealso [`tag_read()`], [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#edit-activity-on-trainset)
#' @export
trainset_read_df <- function(df,
                             directory,
                             filename,
                             series = NA,
                             timestamp = "date",
                             label = "label") {
  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(is.character(timestamp))
  assertthat::assert_that(is.character(label))
  assertthat::assert_that(assertthat::has_name(df, timestamp))
  assertthat::assert_that(is.character(directory))
  assertthat::assert_that(is.character(filename))
  assertthat::assert_that(dir.exists(directory))
  fullpath <- file.path(directory, filename)
  assertthat::assert_that(file.exists(fullpath))

  if (!assertthat::has_name(df, label)){
    df[[label]] <- ""
  }

  # read the file
  csv <- utils::read.csv(fullpath)

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

  # Find the corresponding time
  id_match <- match(as.numeric(df[[timestamp]]), as.numeric((csv$date)))

  # use label only if not NA (missing, see below for warning message)
  df[[label]][!is.na(id_match)] <- csv$label[id_match[!is.na(id_match)]]

  missing_pres <- sum(is.na(id_match))

  if (missing_pres > 0) {
    warning(paste0(
      "The labelization file is missing ", missing_pres, " timesteps and includes ",
      nrow(csv) - nrow(df) + missing_pres, " timestep which are not nedded. ",
      "We assumed no migration and no outlier during the timestep missing."
    ))
  }

  return(df)
}
