#' Start the GeoPressureViz shiny app
#'
#' GeoPressureViz is a shiny app designed to helps you visualize the overall trajectory of the bird
#' as well as each step-by-step move. Learn more about GeoPressureViz in the [GeoPressureManual |
#' GeoPressureViz](https://raphaelnussbaumer.com/GeoPressureManual/geopressureviz.html) or with
#' a [demo of the Great Reed Warbler (18LX)](https://rafnuss.shinyapps.io/GeoPressureViz/).
#'
#' @param pam PAM logger dataset list with `pam$sta` computed. See [`pam_read()`] and [`pam_sta()`].
#' @param static_prob List of raster containing probability map of each stationary period. The
#' metadata of `static_prob` needs to include the flight information to the next stationary period
#' in the metadata `flight`. See [GeoPressureManual | Static map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/static-map.html#combine-pressure-and-light).
#' @param pressure_prob List of raster containing probability map of each stationary period
#' according to pressure data. See [GeoPressureManual | Pressure map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#compute-probability-maps).
#' @param light_prob List of raster containing probability map of each stationary period according
#' to light data. See [GeoPressureManual | Light map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/light-map.html#compute-probability-map).
#' @param static_prob_marginal List of raster containing probability map of each stationary period
#' according to the graph output. See [GeoPressureManual | Basic graph
#' ](https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#output-2-marginal-probability-map).
#' @param pressure_prob_thr List of raster containing probability map of each stationary period
#' according to the threshold of pressure data. See [GeoPressureManual | Pressure map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#computing-pressure-maps).
#' @param pressure_prob_mismatch List of raster containing probability map of each stationary period
#' according to mismatch of pressure data. See [GeoPressureManual | Pressure map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#computing-pressure-maps).
#' @param pressure_timeserie List of data.frame containing at least `sta_id`, `date` and `pressure0`
#' @param lauch_browser If true (default), the app run in your browser, otherwise on Rstudio
#' @return The path modified in the app.
#' @seealso [GeoPressureManual | GeoPressureViz
#' ](https://raphaelnussbaumer.com/GeoPressureManual/geopressureviz.html)
#' @examples
#' \dontrun{
#' load("data/1_pressure/18LX_pressure_prob.Rdata")
#' load("data/2_light/18LX_light_prob.Rdata")
#' load("data/3_static/18LX_static_prob.Rdata")
#' geopressureviz(
#'   pam = pam,
#'   static_prob = static_prob,
#'   pressure_prob = pressure_prob,
#'   light_prob = light_prob,
#'   pressure_timeserie = static_timeserie
#' )
#' }
#' @export
geopressureviz <- function(pam,
                           static_prob,
                           pressure_prob = NA,
                           light_prob = NA,
                           static_prob_marginal = NA,
                           pressure_prob_thr = NA,
                           pressure_prob_mismatch = NA,
                           pressure_timeserie = NA,
                           lauch_browser = TRUE) {

  # Add possible map to display
  map_choices <- c()
  map_val <- list()
  sta_static <- unlist(lapply(static_prob, function(x) raster::metadata(x)$sta_id))
  if (any(!is.na(light_prob))) {
    map_choices <- c(map_choices, "Light")
    sta_tmp <- unlist(lapply(light_prob, function(x) raster::metadata(x)$sta_id))
    map_val[[length(map_val) + 1]] <- light_prob[sta_tmp %in% sta_static]
  }
  if (any(!is.na(pressure_prob_mismatch))) {
    map_choices <- c(map_choices, "Pressure mis.")
    sta_tmp <- unlist(lapply(pressure_prob_mismatch, function(x) raster::metadata(x)$sta_id))
    map_val[[length(map_val) + 1]] <- pressure_prob_mismatch[sta_tmp %in% sta_static]
  }

  if (any(!is.na(pressure_prob_thr))) {
    map_choices <- c(map_choices, "Pressure thres.")
    sta_tmp <- unlist(lapply(pressure_prob_thr, function(x) raster::metadata(x)$sta_id))
    map_val[[length(map_val) + 1]] <- pressure_prob_thr[sta_tmp %in% sta_static]
  }
  if (any(!is.na(pressure_prob))) {
    map_choices <- c(map_choices, "Pressure")
    sta_tmp <- unlist(lapply(pressure_prob, function(x) raster::metadata(x)$sta_id))
    map_val[[length(map_val) + 1]] <- pressure_prob[sta_tmp %in% sta_static]
  }
  map_choices <- c(map_choices, "Static")
  map_val[[length(map_val) + 1]] <- static_prob
  if (any(!is.na(static_prob_marginal))) {
    map_choices <- c(map_choices, "Marginal")
    sta_tmp <- unlist(lapply(light_prob, function(x) raster::metadata(x)$sta_id))
    map_val[[length(map_val) + 1]] <- static_prob_marginal[sta_tmp %in% sta_static]
  }

  # Get stationary period information
  sta <- do.call("rbind", lapply(static_prob, function(r) {
    mt <- raster::metadata(r)
    mt$start <- mt$temporal_extent[1]
    mt$end <- mt$temporal_extent[2]
    # mt$duration <- as.numeric(difftime(mt$end, mt$start, units = "days"))
    as.data.frame(mt[!(names(mt) %in% c("flight", "temporal_extent", "max_sample", "margin"))])
  }))

  # Check pam
  pressure <- pam$pressure
  assertthat::assert_that(is.data.frame(pressure))
  assertthat::assert_that(assertthat::has_name(pressure, c("date", "obs", "sta_id")))
  assertthat::assert_that(inherits(pressure$date, "POSIXt"))
  assertthat::assert_that(is.numeric(pressure$obs))

  if (!assertthat::has_name(pressure, "isoutlier")) {
    if (assertthat::has_name(pressure, "isoutliar")) {
      warning(
        "pressure$isoutliar is deprecated in favor of pressure$isoutlier. This code will continue",
        " but update your code and data to be compatible with futur version of GeoPressureR."
      )
      pressure$isoutlier <- pressure$isoutliar
    } else {
      assertthat::assert_that(assertthat::has_name(pressure, "isoutlier"))
    }
  }
  gdl_id <- pam$id


  # Correct duration for pressure datapoint available
  pres_outlier_sta <- stats::aggregate(!pressure$isoutlier,
    by = list(sta_id = pressure$sta_id),
    FUN = sum
  )
  res <- as.numeric(difftime(pressure$date[2], pressure$date[1], units = "days"))
  id_match <- match(sta$sta_id, pres_outlier_sta$sta_id)
  assertthat::assert_that(all(!is.na(id_match)))
  sta$duration <- pres_outlier_sta$x[id_match] * res

  # Set color of each stationary period
  col <- rep(RColorBrewer::brewer.pal(8, "Dark2"), times = ceiling(max(sta$sta_id) / 8))
  sta$col <- col[sta$sta_id]

  # Get flight information and compute flight duration directly
  flight <- lapply(static_prob, function(r) {
    fl <- raster::metadata(r)$flight
    if (length(fl) > 0) {
      fl$duration <- mapply(function(s, e) {
        as.numeric(difftime(e, s, units = "hours"))
      }, fl$start, fl$end)
    } else {
      fl$duration <- 0
    }
    fl
  })



  # Get the pressure timeserie
  if (any(!is.na(pressure_timeserie))) {
    assertthat::assert_that(length(pressure_timeserie) == nrow(sta))
    # TODO Assert pressure_timeserie is correct
    p_ts_sta_id <- unlist(lapply(pressure_timeserie, function(x) {
      if (is.null(x)) {
        NA
      } else {
        stats::median(x$sta_id[!(x$sta_id == 0)])
      }
    }))
    test <- p_ts_sta_id == sta$sta_id
    test[is.na(test)] <- TRUE
    assertthat::assert_that(all(test))
    ts0 <- pressure_timeserie
    ts0 <- lapply(ts0, function(x) {
      if (is.null(x)) {
        x <- data.frame(
          lon = NA,
          lat = NA,
          lt = 1,
          sta_id = 0
        )
      } else {
        x$lt <- 1
      }
      return(x)
    })
    path0 <- do.call("rbind", lapply(ts0, function(x) {
      data.frame(
        lon = x$lon[1],
        lat = x$lat[1],
        sta_id = stats::median(x$sta_id)
      )
    }))
  } else {
    ts0 <- list()
  }

  if (!exists("path0")) {
    # Set the initial path to the most likely from static prob
    path0 <- geopressure_map2path(static_prob)
  }



  # PEROSENVIR <- new.env(parent=emptyenv())
  .GlobalEnv$.map_choices <- map_choices
  .GlobalEnv$.map_val <- map_val
  .GlobalEnv$.sta <- sta
  .GlobalEnv$.pressure <- pressure
  .GlobalEnv$.gdl_id <- gdl_id
  .GlobalEnv$.ts0 <- ts0
  .GlobalEnv$.path0 <- path0
  .GlobalEnv$.flight <- flight

  # delete variable when removed
  on.exit(
    rm(
      list = c(".map_choices", ".map_val", ".sta", ".pressure", ".ts0", ".path0", ".flight"),
      envir = .GlobalEnv
    )
  )

  if (lauch_browser) {
    lauch_browser <- getOption("browser")
  } else {
    lauch_browser <- getOption("shiny.launch.browser", interactive())
  }

  # Start the app
  shiny::runApp(system.file("geopressureviz", package = "GeoPressureR"),
    launch.browser = lauch_browser
  )
  .GlobalEnv$.path0
}
