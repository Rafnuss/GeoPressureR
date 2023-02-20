#' Path to data.frame
#'
#' This function convert a GeoPressureR path to a data.frame which can be read in [movevis](
#' https://movevis.org/index.html) for instance.
#'
#' The function basically duplicate location position at the start and end time of each stationary
#' period.
#'
#' @param tag data logger dataset list with `tag$sta` computed (see `tag_sta`)
#' @param path data.frame containtings the path(s) of the bird with column `lat`, `lon` and `stap`
#' at least. Path can be generated with `map2path`, `graph_simulation`, `geopressureviz`
#' .
#'
#' @examples
#' tag <- tag_read(
#'   directory = system.file("extdata/0_tag/18LX", package = "GeoPressureR")
#' )
#' tag <- trainset_read(tag,
#'   directory = system.file("extdata/1_pressure/labels", package = "GeoPressureR")
#' )
#' tag <- tag_stap(tag)
#' pressure_likelihood_1 <- readRDS(system.file("extdata/1_pressure/", "
#'   18LX_pressure_likelihood_1.rda",
#'   package = "GeoPressureR"
#' ))
#' path <- map2path(list(pressure_likelihood_1))
#' path2df(tag, path)
#' @seealso [`movevis::df2move`](https://movevis.org/reference/df2move.html),
#' [`map2path`], [`graph_simulation`], [`geopressureviz`]
#' @export
path2df <- function(tag, path) {
  # Check input variable
  assertthat::assert_that(is.list(tag))
  assertthat::assert_that(assertthat::has_name(tag, "sta"))
  assertthat::assert_that(is.data.frame(tag$sta))
  assertthat::assert_that(assertthat::has_name(tag$sta, c("stap", "start", "end")))

  assertthat::assert_that(is.list(path))
  assertthat::assert_that(assertthat::has_name(path, c("lat", "lon", "stap")))

  if (is.matrix(path$lat)) {
    df0 <- data.frame(
      lat = utils::stack(as.data.frame(t(path$lat)))$values,
      lon = utils::stack(as.data.frame(t(path$lon)))$values,
      stap = rep(path$stap, dim(path$lat)[1]),
      track_id = glue::glue(tag$id, "_", rep(seq(1, dim(path$lat)[1]), dim(path$lat)[2]))
    )
    df0 <- merge(df0, tag$sta, by = "stap")
  } else {
    df0 <- merge(as.data.frame(path), tag$sta, by = "stap")
    df0$track_id <- tag$id
  }

  df1 <- df0
  df1$time <- df1$start
  df2 <- df0
  df2$time <- df2$end

  df <- rbind(df1, df2)
  df[, names(df) %in% c("time", "track_id", "lat", "lon")]
}
