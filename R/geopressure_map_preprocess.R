#' Preprocess pressure data
#'
#' @description
#' Performs the follow pre-processing for the pressure data.frame:
#'
#' 1. Remove discarded and flight pressure measurements.
#' 2. Group pressure measurements by stationary period and elevation level.
#' 3. Smooth and downscale pressure to a 1 hour resolution to match ERA5 data.
#'
#' This function is used within [`geopressure_map_mismatch()`] but can be useful to check the
#' pressure data sent to the GeoPressureAPI.
#' @inheritParams geopressure_map
#' @param stap Stationary period information (typically `geostap$stap` or `tag$stap`).
#' @return Pressure data.frame without flight and discarded values, on a 1hr resolution.
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#' tag <- tag_read("18LX") |>
#'   tag_label()
#'
#' pressure_processed <- geopressure_map_preprocess(tag$pressure, tag$stap)
#' str(pressure_processed)
#' @export
geopressure_map_preprocess <- function(pressure, stap) {
  assertthat::assert_that(is.data.frame(pressure))
  assertthat::assert_that(assertthat::has_name(pressure, c("date", "value", "label", "stap_id")))
  assertthat::assert_that(assertthat::is.time(pressure$date))
  assertthat::assert_that(is.numeric(pressure$value))
  assertthat::assert_that(nrow(pressure) >= 3)
  assertthat::assert_that(min(diff(as.numeric(pressure$date))) / 60 / 60 <= 1)

  assertthat::assert_that(is.data.frame(stap))
  assertthat::assert_that(assertthat::has_name(stap, "stap_id"))
  assertthat::assert_that(assertthat::has_name(stap, "include"))

  if (length(unique(diff(pressure$date))) > 1) {
    cli::cli_warn("Pressure data is not on a regular interval.The code should still
    technically work, but it might be the cause of an error later.")
  }

  if ("include" %in% names(stap)) {
    # Filter stap to model
    pressure <- pressure[pressure$stap_id %in% stap$stap_id[stap$include], ]
  }

  if ("known_lat" %in% names(stap)) {
    # Filter stap which are known (i.e, doesn't have lat, lon)
    pressure <- pressure[pressure$stap_id %in% stap$stap_id[is.na(stap$known_lat)], ]
  }

  # remove flight and discard label
  pressure <- pressure[pressure$label != "flight" &
    pressure$label != "discard" &
    pressure$stap_id > 0, ]

  if (max(pressure$date) > Sys.time() - 3 * 30 * 24 * 60 * 60) {
    cli::cli_warn("There are potentially not yet pressure data on the Google Earth \\
                           Engine server for the latest stationary period. Please allow for \\
                           around 3 months before the data becomes available")
  }

  if (min(pressure$value, na.rm = TRUE) < 250 || 1100 < max(pressure$value, na.rm = TRUE)) {
    cli::cli_warn("Pressure observation should be between 250 hPa (~10000m) and 1100 hPa \\
    (sea level at 1013hPa). Check unit returned by {.fun tag_read}.")
  }

  # Create the stapelev of pressure to query: stationary period and elevation
  pressure$stapelev <- paste(pressure$stap_id,
    ifelse(startsWith(pressure$label, "elev_"),
      gsub("^.*?elev_", "", pressure$label),
      "0"
    ),
    sep = "|"
  )

  # Split the data.frame per stapelev
  pressure_stapelev <- split(pressure, pressure$stapelev)

  # Smooth and downscale each stapelev
  pressure_stapelev_clean <- lapply(pressure_stapelev, function(pgi) {
    # Define a regular temporal grid for smoothing and down scaling, rounded to the hours
    date_reg <- seq(
      round.POSIXt(min(pgi$date), units = "hours"),
      round.POSIXt(max(pgi$date), units = "hours"),
      by = min(diff(pgi$date))
    )

    # Remove observation outside the start and end time. This should only be 1 or 2 datapoints
    pgi <- pgi[pgi$date >= date_reg[1] & pgi$date <= date_reg[length(date_reg)], ]

    # Re-sample to the new temporal grid
    id <- sapply(pgi$date, function(d) {
      which.min(abs(d - date_reg))
    })
    assertthat::assert_that(length(id) == length(unique(id)))
    assertthat::assert_that(all(difftime(pgi$date, date_reg[id], units = "hours") <= 0.5))
    pgi$date <- date_reg[id]

    # Create the dataset on the new grid, allowing for NA if no data available
    pgi_reg <- merge(
      data.frame(date = date_reg),
      pgi,
      by = "date",
      all.x = TRUE
    )

    # smooth the data with a moving average of 1hr
    # find the size of the windows for 1 hour
    dtall <- diff(pgi_reg$date)
    units(dtall) <- "hours"
    dt <- as.numeric(stats::median(dtall))
    n <- round(1 / dt + 1)

    # check that there are enough datapoint for the smoothing
    if (nrow(pgi_reg) > n) {
      smoothna <- stats::filter(
        c(FALSE, !is.na(pgi_reg$value), FALSE),
        rep(1 / n, n)
      )
      pgi_reg$value[is.na(pgi_reg$value)] <- 0
      smooth <- stats::filter(c(0, pgi_reg$value, 0), rep(1 / n, n))

      tmp <- smooth / smoothna
      tmp <- tmp[seq(2, length(tmp) - 1)]

      pgi_reg$value <- tmp
    }

    # downscale to 1 hour
    # Pressure is an instantaneous parameters
    # (https://confluence.ecmwf.int/display/CKB/Parameters+valid+at+the+specified+time), so we take
    # the value at the exact hour
    pgi_reg <- pgi_reg[seq(1, nrow(pgi_reg), by = 1 / dt), ]

    # Remove time without measure
    pgi_reg <- pgi_reg[!is.na(pgi_reg$stap_id), ]

    return(pgi_reg)
  })

  # Combine into a single data.frame
  pressure <- do.call("rbind", pressure_stapelev_clean)

  if (nrow(pressure) == 0) {
    cli::cli_abort(c(
      x = "There are no pressure data to match.",
      i = "Check the input pressure label and stap."
    ))
  }
  assertthat::assert_that(all(!is.na(pressure$date)))
  assertthat::assert_that(all(!is.na(pressure$value)))
  assertthat::assert_that(all(pressure$stapelev != ""))
  if ("include" %in% names(stap)) {
    stap <- stap[stap$include & is.na(stap$known_lat), ]
  }
  stap <- merge(stap,
    as.data.frame(table(pressure$stap_id)),
    by.x = "stap_id", by.y = "Var1", all.x = TRUE
  )
  stap$Freq[is.na(stap$Freq)] <- 0

  if (any(stap$Freq < 3)) {
    cli::cli_warn("The following stationary period {.var {stap$stap_id[stap$Freq<3]}} have less \\
                  than 3 datapoints to be used")
  }

  return(pressure)
}
