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
                                file = glue::glue("./data/twilight-label/{tag$param$id}-labeled.csv")) {
  tag_assert(tag, "twilight")

  tag$twilight <- trainset_read(tag$twilight,
    file = file,
    timestamp = "twilight"
  )
  return(tag)
}
