#' Export csv label file
#'
#' @description
#' This function writes the csv file of labelled activity and pressure which can
#' be read with [TRAINSET](https://trainset.geocene.com/). If no label data exist, it will first
#' initialise the label data.
#'
#' Optionally, it can also export a reference dataset for pressure `tag$pressure$ref` as another
#' series to be visualized on TRAINSET, but without impacting the labelling process.
#'
#' @inheritParams tag_label
#' @param file Absolute or relative path of the label file to be saved.
#'
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#' tag <- tag_read("18LX", quiet = T)
#'
#' # Writing unlabeled tag will initialize the labelling for trainset
#' file <- tag_label_write(tag)
#' str(read.csv(file))
#'
#' # Writing unlabeled tag will initialize the labelling for trainset
#' tag <- tag_label_auto(tag)
#' file <- tag_label_write(tag)
#' str(read.csv(file))
#'
#' # Writing labeled tag will use the existing labels
#' tag <- tag_label(tag)
#' file <- tag_label_write(tag,
#'   file = glue::glue("./data/tag-label/{tag$param$id}-v2.csv")
#' )
#' str(read.csv(file))
#'
#' @family tag_label
#' @export
tag_label_write <- function(tag,
                            file = glue::glue("./data/tag-label/{tag$param$id}.csv")) {
  tag_assert(tag)

  # Create empty label if it doesn't exit
  if (!assertthat::has_name(tag$pressure, "label")) {
    tag <- tag_label_auto(tag)
    cli::cli_inform(c(
      "i" = "No label data.",
      ">" = "Initialize automatically label using {.fn tag_label_auto}"
    ))
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
        ">" = "Initialize acceleration label with default {.fn tag_label_auto}"
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
    file = file
  )
  return(invisible(file))
}
