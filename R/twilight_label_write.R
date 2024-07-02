#' Write a twilight label file
#'
#' This function writes the csv file of the labelled twilight which can be read with
#' TRAINSET <https://trainset.raphaelnussbaumer.com/>.
#'
#' @param tag a GeoPressureR `tag` object
#' @param file Name of the twilight label file to be saved.
#' @param quiet logical to hide messages
#'
#' @return None
#'
#' @examples
#' setwd(system.file("extdata", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = TRUE) |>
#'   tag_label(quiet = TRUE) |>
#'   twilight_create()
#'
#' label_file <- twilight_label_write(tag)
#'
#' str(read.csv(label_file))
#' @family geolight
#' @export
twilight_label_write <- function(tag,
                                 file = glue::glue("./data/twilight-label/{tag$param$id}.csv"),
                                 quiet = FALSE) {
  # Check twilight
  tag_assert(tag, "twilight")

  # Extract twilight to convinience
  twilight <- tag$twilight

  # Adapt variable
  twilight$series <- ifelse(twilight$rise, "Rise", "Set")
  twilight$value <- as.numeric(format(twilight$twilight, "%H")) * 60 +
    as.numeric(format(twilight$twilight, "%M"))

  # Normalize the value with offset to be more smooth
  twilight$value <- (twilight$value - tag$param$twl_offset) %% (24 * 60)

  if (!assertthat::has_name(twilight, "label")) {
    if (assertthat::has_name(twilight, "stap_id")) {
      twilight$label <- twilight$stap_id
    } else {
      twilight$label <- ""
    }
  }

  if (any(is.na(twilight$label))) {
    cli::cli_warn(c(
      "!" = "Some twilight label contain NA value",
      "i" = "Check {.code twilight$label} or {.code twilight$stap}",
      ">" = "We replaced them by {.val 'discard'}"
    ))
    twilight$label[is.na(twilight$label)] <- "discard"
  }


  # write a combined data.frame of pressure and acceleration in csv.
  file <- trainset_write(
    twilight,
    file = file,
    timestamp = "twilight",
    quiet = quiet
  )
  invisible(file)
}
