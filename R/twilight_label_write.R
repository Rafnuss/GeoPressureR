#' Write twilight label file
#'
#' This function writes the csv file of the labelled twilight which can be read with
#' TRAINSET <https://trainset.raphaelnussbaumer.com/>.
#'
#' @param tag A GeoPressureR `tag` object
#' @param file Name of the twilight label file to be saved.
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#' tag <- tag_read("18LX", quiet = T) |> tag_label(quiet = T) |> twilight_create()
#'
#' label_file <- twilight_label_write(tag)
#' str(read.csv(label_file))
#' @family twilight
#' @export
twilight_label_write <- function(tag,
                                 file = glue::glue("./data/twilight-label/{tag$param$id}.csv")) {
  # Check twilight
  tag_assert(tag, "twilight")

  # Extract twilight to convinience
  twilight <- tag$twilight

  # Adapt variable
  twilight$series <- ifelse(twilight$rise, "Rise", "Set")
  twilight$value <- (as.numeric(format(twilight$twilight, "%H")) * 60 +
    as.numeric(format(twilight$twilight, "%M")) - tag$param$twl_offset / 60 + 60 * 12) %% (60 * 24)

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
