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
#' @param quiet logical to hide messages
#' @seealso [`tag_label_write()`]
#' @noRd
trainset_write <- function(df,
                           file,
                           series = "series",
                           timestamp = "date",
                           value = "value",
                           label = "label",
                           quiet = FALSE) {
  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(assertthat::has_name(df, c(timestamp, value)))
  assertthat::assert_that(is.character(file))

  # Create empty label if label doesn't exit
  if (!assertthat::has_name(df, label)) {
    df[[label]] <- ""
  }

  # Remove row with empty element
  df <- df[!is.na(df[[value]]), ]

  # Convert to charachter and ensure that there are no dots (trainset doesn't like them)
  df[[label]] <- gsub("\\.", "-", as.character(df[[label]]))


  # Combine the variable
  df_trainset <- data.frame(
    series = df[[series]],
    timestamp = strftime(df[[timestamp]], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    value = df[[value]],
    label = df[[label]]
  )

  # Check if folder exist
  dir_file <- dirname(file)
  if (!dir.exists(dir_file)) {
    cli::cli_inform(c("!" = "The directory {.file {dir_file}} does not \\
                      exists.\f"))
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

  if (!quiet) {
    cli::cli_inform(c("v" = "{.file {file}} written successfully.\f"))
  }
  return(file)
}
