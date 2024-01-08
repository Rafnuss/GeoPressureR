#' Label a `tag` object
#'
#' @description
#' This function performs the following operations:
#'
#' 1. Read label file with `tag_label_read()` and assign the label to a new column in each sensor
#' data.frame
#' 2. Compute the stationary period `tag$stap` from the label and assign the corresponding `stap_id`
#' on all sensors data.frame with `tag_label_stap()`
#'
#' If the label file does not exist, the function will suggest to create it with
#' `tag_label_write()` and use `tag_label_auto()` if acceleration data exists.
#'
#' @param tag a GeoPressure `tag` object.
#' @param file Absolute or relative path of the label file.
#' @param quiet logical to display message.
#' @inheritDotParams tag_label_stap warning_flight_duration warning_stap_duration quiet
#'
#' @return Same `tag` list with
#'
#' (1) a `stap` data.frame describing the STAtionary Period:
#' - `stap_id` unique identifier in increasing order 1,...,n
#' - `start` start date of each stationary period
#' - `end` end date of each stationary period
#'
#' (2) an additional `label` and `stap_id` column on the sensor data.frame:
#' - `date` datetime of measurement as POSIXt
#' - `value` sensor measurement
#' - `label` indicates the observation to be discarded (`"discard"` and `"flight"`) as well as
#' grouped by elevation layer (`elev_*`)
#' - `stap_id` stationary period of the measurement matching the `tag$stap`.
#' @examples
#' setwd(system.file("extdata", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = TRUE)
#'
#' print(tag)
#'
#' tag <- tag_label(tag)
#'
#' print(tag)
#'
#' # The labeled `tag` contains additional column on the sensor data.frame
#' str(tag)
#'
#' @family tag_label
#' @seealso [GeoPressureManual
#' ](https://raphaelnussbaumer.com/GeoPressureManual/labelling-tracks.html)
#' @export
tag_label <- function(tag,
                      file = glue::glue("./data/tag-label/{tag$param$id}-labeled.csv"),
                      quiet = FALSE,
                      ...) {
  tag_assert(tag)
  assertthat::assert_that(is.character(file))

  # Check if the label file exist
  if (!file.exists(file)) {
    # Check if the exported file already exist, in which case it hasn't been edited on trainset
    file_input <- file.path(dirname(file), glue::glue("{tag$param$id}.csv"))
    if (file.exists(file_input)) {
      cli::cli_abort(c(
        "!" = "The label file {.file {file}} does not exist but {.file {file_input}} exist.",
        i = "Edit {.file {file_input}} in TRAINSET and export {.file {file}} in the same
        directory."
      ))
    }

    # Suggest to write the file
    file_default <- glue::glue("./data/tag-label/{tag$param$id}.csv")
    cli::cli_inform(c("!" = "The label file {.file {file}} does not exist.\f"))
    choices <- list(
      "1" = "No",
      "2" = glue::glue("Yes, in `{file_default}` (default)"),
      "3" = glue::glue("Yes, in `{file_input}` (in input file directory)")
    )
    res <- as.numeric(names(utils::select.list(choices, title = "Do you want to create it?")))

    if (res == 2) {
      tag_label_write(tag, file_default, quiet = quiet)
    } else if (res == 3) {
      tag_label_write(tag, file_input, quiet = quiet)
    }

    # Stop the function
    cli::cli_warn(c(
      ">" = "Return the original {.var tag} unmodified.\f"
    ))
    return(tag)
  } else {
    # Check if label has already been setmap
    if ("setmap" %in% tag_status(tag)) {
      cli::cli_inform(c("!" = "The setmap has already been defined for {.var tag}.\f"))
      choices <- list(
        "1" = glue::glue("No, return the original `tag`"),
        "2" = glue::glue("Yes, read the new label, but start `tag` from scratch"),
        "3" = glue::glue("Yes, use `tag_update()` to keep the setmap parameters and re-run \\
                         likelihood maps.")
      )
      res <- as.numeric(names(
        utils::select.list(choices, title = "How to you want to proceed with the new label file?")
      ))

      if (res == 1) {
        return(tag)
      } else if (res == 3) {
        tag <- tag_update(tag, file)
        return(tag)
      } else if (res == 2) {
        tag <- tag_create(
          id = tag$param$id,
          pressure_file = tag$param$pressure_file,
          light_file = tag$param$light_file,
          acceleration_file = tag$param$acceleration_file,
          crop_start = tag$param$crop_start,
          crop_end = tag$param$crop_end,
          quiet = TRUE
        )
      }
    }

    # If the file exist, read it
    tag <- tag_label_read(tag, file)

    # Add the stationary periods
    tag <- tag_label_stap(tag, quiet = quiet, ...)

    return(tag)
  }
}
