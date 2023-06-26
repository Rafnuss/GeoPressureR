#' Add label to tag
#'
#' @description
#' This function performs the following operations:
#'
#' 1. Read label file with [`tag_label_read()`] and assign the label to a new column in each sensor
#' data.frame
#' 2. Compute the stationary period `tag$stap` from the label and assign the corresponding `stap_id`
#' on all sensors data.frame with [`tag_label_stap()`]
#'
#' If the label file does not exist, the function will suggest to create it with
#' [`tag_label_write()`] and use [`tag_label_auto()`] if acceleration data exists.
#'
#' @param tag List containing the data logger dataset, this needs to contain at least a `pressure`
#' data.frame, but can also have a `light` and `acceleration` data.frame (see [`tag_create()`]).
#' @param file Absolute or relative path of the label file.
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
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#' tag <- tag_create("18LX")
#'
#' tag <- tag_label(tag)
#' str(tag)
#'
#' @seealso [`tag_create()`], [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#edit-activity-on-trainset)
#' @family tag_label
#' @export
tag_label <- function(tag,
                      file = glue::glue("data/1-tag_label/{tag$id}-labeled.csv")) {
  assertthat::assert_that(inherits(tag, "tag"))
  assertthat::assert_that(is.character(file))

  # Check if the label file exist
  if (!file.exists(file)) {
    # Check if the exported file already exist, in which case it hasn't been edited on trainset
    file_input <- file.path(dirname(file), glue::glue("{tag$id}.csv"))
    if (file.exists(file_input)) {
      cli::cli_abort(c(
        "!" = "The label file {.file {file}} does not exist but {.file {file_input}} exist.",
        i = "Edit {.file {file_input}} in TRAINSET and export {.file {file}} in the same
        directory."
      ))
    }

    # Suggest to write the file
    file_default <- glue::glue("data/1-tag_label/{tag$id}.csv")
    choices <- c(
      "No",
      glue::glue("Yes, in `{file_default}` (default)"),
      glue::glue("Yes, in `{file_input}` (in input file directory)"),
      "No, I'll create it myself with `tag_label_write()`."
    )
    cli::cli_alert_warning("The label file {.file {file}} does not exist.")
    res <- utils::select.list(choices, title = "Do you want to create it?")
    if (file_default %in% res) {
      tag_label_write(tag, file_default)
    } else if (file_input %in% res) {
      tag_label_write(tag, file_input)
    }

    # Stop the function
    cli::cli_warn(c(
      "!" = "No label file available.",
      ">" = "Return the original {.var tag} unmodified."
    ))
  } else {
    # If the file exist, read it
    tag <- tag_label_read(tag, file)

    # Add the stationary periods
    tag <- tag_label_stap(tag)
  }
  return(invisible(tag))
}
