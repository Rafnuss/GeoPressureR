#' Progress bar function
#' @param x value of the current status
#' @param max maximum value that x will reach
#' @param text text to display
#' @noRd

progress_bar <- function(x, max = 100, text = "") {
  percent <- x / max * 100
  cat(sprintf(
    "\r[%-50s] %d / %d %s",
    paste(rep("=", percent / 2), collapse = ""),
    x, max, text
  ))
  if (x == max) {
    cat("\n")
  }
}

#' Path to data.frame
#'
#' This function convert a GeoPressureR path to a data.frame which can be read in [movevis](
#' https://movevis.org/index.html) for instance.
#'
#' The function basically duplicate location position at the start and end time of each stationary
#' period.
#'
#' @param pam pam logger dataset list with `pam$sta` computed (see `pam_sta`)
#' @param path data.frame containtings the path(s) of the bird with column `lat`, `lon` and `sta_id`
#' at least. Path can be generated with `geopressure_map2path`, `graph_simulation`, `geopressureviz`
#' .
#'
#' @examples
#' pam <- pam_read(
#'   pathname = system.file("extdata/0_PAM/18LX", package = "GeoPressureR")
#' )
#' pam <- trainset_read(pam,
#'   pathname = system.file("extdata/1_pressure/labels", package = "GeoPressureR")
#' )
#' pam <- pam_sta(pam)
#' pressure_prob_1 <- readRDS(system.file("extdata/1_pressure/", "18LX_pressure_prob_1.rda",
#'   package = "GeoPressureR"
#' ))
#' path <- geopressure_map2path(list(pressure_prob_1))
#' path2df(pam, path)
#' @seealso [`movevis::df2move`](https://movevis.org/reference/df2move.html),
#' [`geopressure_map2path`], [`graph_simulation`], [`geopressureviz`]
#' @export
path2df <- function(pam, path) {

  # Check input variable
  assertthat::assert_that(is.list(pam))
  assertthat::assert_that(assertthat::has_name(pam, "sta"))
  assertthat::assert_that(is.data.frame(pam$sta))
  assertthat::assert_that(assertthat::has_name(pam$sta, c("sta_id", "start", "end")))

  assertthat::assert_that(is.list(path))
  assertthat::assert_that(assertthat::has_name(path, c("lat", "lon", "sta_id")))

  if (is.matrix(path$lat)) {
    df0 <- data.frame(
      lat = utils::stack(as.data.frame(t(path$lat)))$values,
      lon = utils::stack(as.data.frame(t(path$lon)))$values,
      sta_id = rep(path$sta_id, dim(path$lat)[1]),
      track_id = paste0(pam$id, "_", rep(seq(1, dim(path$lat)[1]), dim(path$lat)[2]))
    )
    df0 <- merge(df0, pam$sta, by = "sta_id")
  } else {
    df0 <- merge(as.data.frame(path), pam$sta, by = "sta_id")
    df0$track_id <- pam$id
  }

  df1 <- df0
  df1$time <- df1$start
  df2 <- df0
  df2$time <- df2$end

  df <- rbind(df1, df2)
  df[, names(df) %in% c("time", "track_id", "lat", "lon")]
}
