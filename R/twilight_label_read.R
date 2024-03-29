#' Read a twilight label file
#'
#' This function read an exported csv file from TRAINSET <https://trainset.raphaelnussbaumer.com/>
#' and update the twilight data `tag$twilight`.
#'
#' @param tag a GeoPressureR `tag` object
#' @param file csv file of the labels to be read.
#' @return Same `tag` object, updated with the labels `tag$twilight$label`.
#' @family geolight
#' @examples
#' owd <- setwd(system.file("extdata", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = TRUE) |>
#'   tag_label(quiet = TRUE) |>
#'   twilight_create()
#'
#' tag_labelled <- twilight_label_read(tag)
#' setwd(owd)
#'
#' plot(tag, type = "twilight") + ggplot2::ggtitle("Before label")
#'
#' str(tag_labelled$twilight)
#'
#' plot(tag_labelled, type = "twilight") + ggplot2::ggtitle("After label")
#' @export
twilight_label_read <- function(
    tag,
    file = glue::glue("./data/twilight-label/{tag$param$id}-labeled.csv")) {
  tag_assert(tag, "twilight")

  tag$twilight <- trainset_read(tag$twilight,
    file = file,
    timestamp = "twilight"
  )

  tag$param$twilight_file <- file

  return(tag)
}
