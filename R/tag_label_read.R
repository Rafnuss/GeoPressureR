#' Read classification of activity and pressure
#'
#' This function reads an exported csv file from [TRAINSET](https://trainset.geocene.com/) and updates
#' the data logger dataset `tag`.
#'
#' @inheritParams tag_label
#' @return Same data logger list as input, updated with the labels `tag$pressure$label` and
#' optionally `tag$acceleration$label`.
#'
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#' tag <- tag_create("18LX")
#'
#' tag <- tag_label_read(tag)
#' str(tag)
#' @family tag_label
#' @export
tag_label_read <- function(tag,
                           file = glue::glue("data/1-tag_label/{tag$id}-labeled.csv")) {
  assertthat::assert_that(is.list(tag))
  assertthat::assert_that(assertthat::has_name(tag, "pressure"))
  assertthat::assert_that(is.data.frame(tag$pressure))
  assertthat::assert_that(assertthat::has_name(tag$pressure, c("date", "value")))

  tag$pressure <- trainset_read(tag$pressure,
    file = file,
    series = "pressure"
  )

  # Check that all label are correct
  unique_label <- unique(tag$pressure$label)
  unique_label <- unique_label[!(unique_label %in% c("flight", "discard", "") |
    startsWith(unique_label, "elev_"))]
  if (length(unique_label) > 0) {
    cli::cli_abort(c(
      x = "The pressure label file contains unknown label: {.val {unique_label}}",
      i = "Correct the label file {.file {file}} to contains only {.val {c('flight', 'discard', 'elev_*')}}"
    ))
  }

  # Extract acceleration label
  if (assertthat::has_name(tag, "acceleration")) {
    assertthat::assert_that(is.data.frame(tag$acceleration))
    assertthat::assert_that(assertthat::has_name(tag$acceleration, c("date", "value")))

    tag$acceleration <- trainset_read(tag$acceleration,
      file = file,
      series = "acceleration"
    )

    # Check that all label are correct
    if ("label" %in% tag$acceleration) {
      unique_label <- unique(tag$acceleration$label)
      unique_label <- unique_label[!(unique_label %in% c("flight", "discard", "") |
        startsWith(unique_label, "elev_"))]
      if (length(unique_label) > 0) {
        cli::cli_abort(c(
          x = "The acceleration label file contains unknown label: {.val {unique_label}}",
          i = "Correct the label file {.file {file}} to contains only {.val {c('flight', 'discard', 'elev_*')}}"
        ))
      }
    }
  }

  return(tag)
}