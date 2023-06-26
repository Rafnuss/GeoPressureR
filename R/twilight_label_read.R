#' Read label file of light
#'
#' This function read an exported csv file from TRAINSET <https://trainset.geocene.com/> and update
#' the data.frame twilight.
#'
#' @param tag List containing the data logger dataset (see [`tag_create()`]) It needs to contain
#' a `twilight` data.frame created with [`twilight_create()`].
#' @param file csv file of the labels to be read.
#' @return Same twilight data.frame as input, updated with the labels `tag$twilight$label`.
#' @family twilight
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#' tag <- tag_create("18LX") |> tag_label()
#' tag <- twilight_create(tag)
#'
#' twilight <- twilight_label_read(twilight)
#' str(twilight)
#' @export
twilight_label_read <- function(tag,
                                file = glue::glue("data/2-twl_label/{tag$id}-labeled.csv")) {
  assertthat::assert_that(inherits(tag, "tag"))
  assertthat::assert_that(is.data.frame(tag$twilight))
  assertthat::assert_that(assertthat::has_name(tag$twilight, c("twilight", "rise")))

  tag$twilight <- trainset_read(tag$twilight,
    file = file,
    timestamp = "twilight"
  )
  return(tag)
}