#' Read label file of light
#'
#' This function read an exported csv file from TRAINSET <https://trainset.raphaelnussbaumer.com/>
#' and update the twilight data `tag$twilight`.
#'
#' @param tag A GeoPressureR `tag` object
#' @param file csv file of the labels to be read.
#' @return Same `tag` object, updated with the labels `tag$twilight$label`.
#' @family twilight
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#'
#' tag <- tag_read("18LX", quiet = T) |> tag_label(quiet = T) |> twilight_create()
#' plot(tag, type="twilight", plot_plotly = F) + ggplot2::ggtitle("Before label")
#'
#' tag <- twilight_label_read(tag)
#' str(tag$twilight)
#' plot(tag, type="twilight", plot_plotly = F) + ggplot2::ggtitle("After label")
#' @export
twilight_label_read <- function(tag,
                                file = glue::glue("./data/twilight-label/{tag$param$id}-labeled.csv")) {
  tag_assert(tag, "twilight")

  tag$twilight <- trainset_read(tag$twilight,
    file = file,
    timestamp = "twilight"
  )

  tag$param$twilight_file <- file

  return(tag)
}
