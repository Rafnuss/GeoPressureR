#' Write a tag label file
#'
#' @description
#' This function writes the csv file of labelled activity and pressure which can
#' be read with [TRAINSET](https://trainset.geocene.com/). If no label data exist, it will first
#' initialize the label data.
#'
#' Optionally, it can also export a reference dataset for pressure `tag$pressure$ref` as another
#' series to be visualized on TRAINSET, but without impacting the labelling process.
#'
#' @inheritParams tag_label
#' @param file Absolute or relative path of the label file to be saved.
#'
#' @return The file pathname is return invisibly
#'
#' @examples
#' setwd(system.file("extdata", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = TRUE)
#'
#' # Writing unlabelled tag will initialize the labelling for trainset
#' tag_label_write(tag)
#'
#' # Writing unlabelled tag will initialize the labelling for trainset
#' tag <- tag_label_auto(tag)
#' tag_label_write(tag)
#'
#' # Writing labelled tag will use the existing labels
#' tag <- tag_label(tag)
#' tag_label_write(tag)
#'
#' @family tag_label
#' @seealso [GeoPressureManual](https://bit.ly/3QC7IBt)
#' @export
tag_label_write <- function(tag,
                            file = glue::glue("./data/tag-label/{tag$param$id}.csv"),
                            quiet = FALSE) {
  tag_assert(tag)

  # Create empty label if it doesn't exit
  if (!assertthat::has_name(tag$pressure, "label")) {
    tag <- tag_label_auto(tag)
    if (!quiet) {
      cli::cli_inform(c(
        "i" = "No label data.",
        ">" = "Initialize automatically label using {.fn tag_label_auto}\f"
      ))
    }
  }

  # Add series name for TRAINSET
  tag$pressure$series <- "pressure"

  # Select only the column needed
  common_column <- c("date", "value", "label", "series")
  df <- tag$pressure[common_column]

  # Add acceleration label if available
  if (assertthat::has_name(tag, "acceleration")) {
    if (!assertthat::has_name(tag$acceleration, "label")) {
      tag <- tag_label_auto(tag)
      cli::cli_inform(c(
        "i" = "No acceleration label data.",
        ">" = "Initialize acceleration label with default {.fn tag_label_auto}\f"
      ))
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
  file <- trainset_write(
    df,
    file = file,
    quiet = quiet
  )
  return(invisible(file))
}
