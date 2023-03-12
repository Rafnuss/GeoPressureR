#' Export twilight label
#'
#' This function writes the csv file of the labelled twilight which can be read with
#' TRAINSET <https://trainset.geocene.com/>.
#'
#' @inheritParams twilight_create
#' @param tag List containing the data logger dataset (see [`tag_read()`]) It needs to contain
#' a `twilight` data.frame created with [`twilight_create()`].
#' @param file Name of the twilight label file to be saved.
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#' tag <- tag_read("18LX") |> tag_label()
#' tag <- twilight_create(tag)
#'
#' label_file <- twilight_label_write(twilight)
#' str(read.csv(label_file))
#' @family twilight
#' @export
twilight_label_write <- function(tag,
                                 file = glue::glue("data/2-twl_label/{tag$id}.csv"),
                                 twl_offset = 0) {
  assertthat::assert_that(is.list(tag))
  twilight <- tag$twilight
  assertthat::assert_that(is.data.frame(twilight))
  assertthat::assert_that(assertthat::has_name(twilight, c("twilight", "rise")))

  # Adapt variable
  twilight$series <- ifelse(twilight$rise, "Rise", "Set")
  twilight$value <- (as.numeric(format(twilight$twilight, "%H")) * 60 +
    as.numeric(format(twilight$twilight, "%M")) - twl_offset / 60 + 60 * 12) %% (60 * 24)

  if (!assertthat::has_name(twilight, "label")) {
    if (assertthat::has_name(twilight, "stap_id")) {
      twilight$label <- twilight$stap_id
    } else {
      twilight$label <- ""
    }
  }

  # write a combined data.frame of pressure and acceleration in csv.
  file <- trainset_write(
    twilight,
    file = file,
    timestamp = "twilight"
  )
  invisible(file)
}
