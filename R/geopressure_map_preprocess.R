#' Prepare pressure data for `geopressure_map()`
#'
#' @description
#' Performs the following pre-processing for the pressure data.frame:
#'
#' 1. Remove discarded and flight pressure measurements.
#' 2. Group pressure measurements by stationary period and elevation level.
#' 3. Smooth and downscale pressure to a 1 hour resolution to match ERA5 data.
#'
#' This function is used within [`geopressure_map_mismatch()`] but can be useful to check the
#' pressure data sent to the GeoPressureAPI.
#' @inheritParams geopressure_map
#' @return Pressure data.frame without flight and discarded values, on a 1 hour resolution.
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |> tag_label(quiet = TRUE)
#' })
#'
#' pressure_processed <- geopressure_map_preprocess(tag)
#' str(pressure_processed)
#'
#' # Plotting pressure displays the raw data (in grey) with the processed data from
#' # `geopressure_map_preprocess` (as coloured line)
#' plot_tag_pressure(tag, quiet = TRUE)
#'
#' @export
geopressure_map_preprocess <- function(tag, compute_known = FALSE) {
  tag_assert(tag, "label")
  stap <- tag$stap
  pressure <- tag$pressure

  if (any(pressure$stap_id == 0)) {
    cli::cli_warn(c(
      "!" = "{.var pressurepath} has been create with an old version of \\
      {.pkg GeoPressureR} (<v3.2.0)",
      ">" = "For optimal performance, we suggest to re-run your code"
    ))
    id <- pressure$stap_id == 0
    sequence <- seq_len(nrow(pressure))
    pressure$stap_id[id] <- stats::approx(
      sequence[!id],
      pressure$stap_id[!id],
      sequence[id]
    )$y
  }

  if (nrow(pressure) < 3) {
    cli::cli_abort(c(
      "x" = "Pressure data has less than {.val {3}} datapoints.",
      "!" = "At least {.val {3}} datapoints of pressure are required."
    ))
  }

  if (min(diff(as.numeric(pressure$date))) / 60 / 60 > 1) {
    cli::cli_abort(c(
      "x" = "The temporal resolution of pressure is greater than {.val {1}} hour.",
      "!" = "A maximal resolution of {.val {1}} hour is required."
    ))
  }

  if (length(unique(diff(pressure$date))) > 1) {
    cli::cli_warn(
      "Pressure data is not on a regular interval. The code should still
    technically work, but it might be the cause of an error later."
    )
  }

  # Filter stap to model
  if ("include" %in% names(stap)) {
    pressure <- pressure[pressure$stap_id %in% stap$stap_id[stap$include], ]
  }

  # Filter stap which are known
  if (!compute_known && "known_lat" %in% names(stap)) {
    pressure <- pressure[
      pressure$stap_id %in% stap$stap_id[is.na(stap$known_lat)],
    ]
  }

  # Remove flight and discard label
  id <- pressure$label != "flight" &
    pressure$label != "discard" &
    pressure$stap_id == round(pressure$stap_id)
  tmp <- unique(pressure$stap_id[
    !(pressure$stap_id %in% pressure$stap_id[id]) &
      pressure$stap_id == round(pressure$stap_id)
  ])
  if (length(tmp) > 0) {
    stap_info <- cli::format_inline({
      for (i in tmp) {
        cli::cli_text(c(
          "*" = "{.val {i}} ({tag$stap$start[i]} - {tag$stap$end[i]}),"
        ))
      }
    })
    cli::cli_abort(c(
      "x" = "All labels of the stationary period(s) {.val {tmp}} are either \\
      {.val discard} or in {.val flight}.",
      "i" = stap_info,
      ">" = "Modify the label on trainset to fix this."
    ))
  }
  pressure <- pressure[id, ]

  if (max(pressure$date) > Sys.time() - 3 * 30 * 24 * 60 * 60) {
    cli::cli_warn(
      "There are potentially not yet pressure data on the Google Earth \\
                           Engine server for the latest stationary period. Please allow for \\
                           around 3 months before the data becomes available."
    )
  }

  if (
    min(pressure$value, na.rm = TRUE) < 250 ||
      1100 < max(pressure$value, na.rm = TRUE)
  ) {
    cli::cli_warn(
      "Pressure observation should be between 250 hPa (~10000m) and 1100 hPa \\
    (sea level at 1013hPa). Check unit returned by {.fun tag_create}."
    )
  }

  # Create the stapelev of pressure to query: stationary period and elevation
  pressure$stapelev <- paste(
    pressure$stap_id,
    ifelse(
      startsWith(pressure$label, "elev_"),
      gsub("^.*?elev_", "", pressure$label),
      "0"
    ),
    sep = "|"
  )

  # Split the data.frame per stapelev
  pressure_stapelev <- split(pressure, pressure$stapelev)

  pressure_stapelev_nrow <- lapply(pressure_stapelev, nrow)
  if (any(pressure_stapelev_nrow <= 1)) {
    cli::cli_abort(
      "There are not enough datapoint in stationary periods {.val \\
                   {names(pressure_stapelev_nrow)[pressure_stapelev_nrow <= 1]}}."
    )
  }

  # Smooth and downscale each stapelev
  # pgi <- pressure_stapelev[[9]]
  pressure_stapelev_clean <- lapply(pressure_stapelev, function(pgi) {
    # Define a regular temporal grid for smoothing and down scaling, rounded to the hours
    dt <- min(diff(pgi$date))
    units(dt) <- "hours"
    date_reg <- seq(
      round.POSIXt(min(pgi$date), units = "hours"),
      round.POSIXt(max(pgi$date), units = "hours"),
      by = dt
    )

    # Remove observation outside the start and end time. This should only be 1 or 2 datapoints
    # pgi <- pgi[pgi$date >= date_reg[1] & pgi$date <= date_reg[length(date_reg)], ]

    # Re-sample to the new temporal grid
    # Too slow
    # id <- sapply(pgi$date, function(d) {
    #   which.min(abs(d - date_reg))
    # })

    id <- round(
      stats::approx(
        x = pgi$date,
        y = seq_len(nrow(pgi)),
        xout = date_reg,
        method = "linear",
        rule = 2
      )$y
    )

    # Create a new data.frame of regular pressure
    pgi_reg <- pgi[id, ]
    # keep old recorded date for info
    pgi_reg$date_ireg <- pgi_reg$date
    # use new date
    pgi_reg$date <- date_reg

    # Only keep value which are within 30min of the actual measurement.
    id <- difftime(pgi_reg$date, pgi_reg$date_ireg, units = "hours") > 0.5
    pgi_reg$value[id] <- NA
    # Also set stap_id NA to be able to identify those time to remove at the end of this function
    pgi_reg$stap_id[id] <- NA

    # smooth the data with a moving average of 1 hour
    # find the size of the windows for 1 hour
    n <- round(1 / as.numeric(dt) + 1)

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

      pgi_reg$value[!is.na(tmp)] <- tmp[!is.na(tmp)]
    }

    # downscale to 1 hour
    # Pressure is an instantaneous parameters
    # (https://confluence.ecmwf.int/display/CKB/Parameters+valid+at+the+specified+time), so we take
    # the value at the exact hour
    pgi_reg <- pgi_reg[seq(1, nrow(pgi_reg), by = 1 / as.numeric(dt)), ]

    # Remove time without measure
    pgi_reg <- pgi_reg[!is.na(pgi_reg$stap_id), ]

    assertthat::assert_that(!anyNA(pgi_reg$value))

    pgi_reg
  })

  # Combine into a single data.frame
  pressure_clean <- do.call("rbind", pressure_stapelev_clean)

  # Make sure pressure is sorted by date (`pressure_stapelev_clean` uses stap_id as string and does
  # not insure the correct order when exceeding 10).
  pressure_clean <- pressure_clean[order(pressure_clean$date), ]

  if (nrow(pressure_clean) == 0) {
    cli::cli_abort(c(
      x = "There are no pressure data to match.",
      i = "Check the input pressure label and stap."
    ))
  }
  assertthat::assert_that(!anyNA(pressure_clean$date))
  assertthat::assert_that(!anyNA(pressure_clean$value))
  assertthat::assert_that(all(pressure_clean$stapelev != ""))

  # Check number of datapoint per stationary period
  stap <- stap[stap$include & is.na(stap$known_lat), ]
  stap <- merge(
    stap,
    as.data.frame(table(pressure_clean$stap_id)),
    by.x = "stap_id",
    by.y = "Var1",
    all.x = TRUE
  )
  stap$Freq[is.na(stap$Freq)] <- 0

  if (any(stap$Freq < 3)) {
    cli::cli_warn(
      "The stationary period {.var {stap$stap_id[stap$Freq<3]}} have less \\
                  than 3 datapoints to be used.\f"
    )
  }

  # delete the internal used `date_ireg`
  pressure_clean$date_ireg <- NULL

  rownames(pressure_clean) <- NULL
  return(pressure_clean)
}
