#' Plot a `pressurepath`
#'
#' Display a `pressurepath` data.frame as a time series or histogram
#'
#' @param pressurepath a GeoPressureR `pressurepath` data.frame.
#' @param type `"timeseries"` (default), `"histogram"`, or `"altitude"`
#' @inheritParams geopressure_map
#' @param warning_std_thr Threshold of outlier, coefficient of the [z-score](
#' https://en.wikipedia.org/wiki/Standard_score)
#' @param plot_plotly logical to use `plotly`
#'
#' @return a plot or ggplotly object.
#'
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |> tag_label(quiet = TRUE)
#' })
#'
#' path <- data.frame(
#'   stap_id = tag$stap$stap_id,
#'   lat = c(48.5, 32.5, 30.5, 49.5, 41.6),
#'   lon = c(17.5, 13.5, 16.5, 21.5, 12.7)
#' )
#'
#' pressurepath <- pressurepath_create(tag, path = path, quiet = TRUE)
#'
#' plot_pressurepath(pressurepath)
#'
#' plot_pressurepath(pressurepath, type = "histogram")
#'
#' plot_pressurepath(pressurepath, type = "altitude")
#'
#' @family pressurepath
#' @export
plot_pressurepath <- function(pressurepath,
                              type = "timeseries",
                              sd = attr(pressurepath, "sd"),
                              warning_std_thr = 3,
                              plot_plotly = TRUE) {
  assertthat::assert_that(is.data.frame(pressurepath))

  if ("pressure_era5" %in% names(pressurepath)) {
    cli::cli_warn(c(
      "!" = "{.var pressurepath} has been create with an old version of \\
      {.pkg GeoPressureR} (<v3.2.0)",
      ">" = "For optimal performance, we suggest to re-run \\
      {.fun pressurepath_create}"
    ))
    pressurepath$surface_pressure <- pressurepath$pressure_era5
    pressurepath$surface_pressure_norm <- pressurepath$pressure_era5_norm

    id <- pressurepath$stap_id == 0
    sequence <- seq_len(nrow(pressurepath))
    pressurepath$stap_id[id] <- stats::approx(
      sequence[!id], pressurepath$stap_id[!id], sequence[id]
    )$y
  }

  assertthat::assert_that(assertthat::has_name(
    pressurepath, c(
      "date", "stap_id", "pressure_tag", "label", "surface_pressure", "altitude", "lat", "lon",
      "surface_pressure_norm"
    )
  ))

  if (is.null(sd)) {
    sd <- rep(1, times = length(unique(pressurepath$stap_id)))
  }
  if (length(sd) == 1) {
    sd <- rep(sd, times = length(unique(pressurepath$stap_id)))
  }

  # Shorter name for the rest of the code
  pp <- pressurepath

  # Group by stapelev rather than stap in order to assess the use of elev
  pp$stapelev <- paste(ifelse(round(pp$stap_id) == pp$stap_id, pp$stap_id, 0),
    ifelse(startsWith(pp$label, "elev_"), gsub("^.*?elev_", "", pp$label), "0"),
    sep = "|"
  )

  # Compute the error
  pp$error <- pp$pressure_tag - pp$surface_pressure_norm

  # Remove error for discard
  pp$error[pp$label == "discard"] <- NA

  # Compute the error std and offset
  tag_era5 <- merge(
    stats::aggregate(
      list(error_sd = pp$error), list(stapelev = pp$stapelev),
      \(x) round(stats::sd(x, na.rm = TRUE), 2)
    ),
    stats::aggregate(
      list(error_offset = pp$surface_pressure - pp$surface_pressure_norm),
      list(stapelev = pp$stapelev), \(x) round(mean(x), 2)
    )
  )
  tag_era5 <- tag_era5[order(as.numeric(gsub("|", ".", tag_era5$stapelev, fixed = TRUE))), ]

  # Add sd and offset to pp
  pp <- merge(pp, tag_era5)
  pp <- pp[order(pp$date), ]

  # knitr::kable(tag_era5, "simple")

  if (type %in% c("timeseries", "ts")) {
    pp$warning <- (abs(pp$error) / sd[ifelse(pp$stap_id == round(pp$stap_id), 1, pp$stap_id)]) >=
      warning_std_thr

    # convert stapelev to factor for color
    pp$stap_id <- factor(pp$stap_id)

    p <- ggplot2::ggplot(pp, ggplot2::aes(x = .data$date)) +
      ggplot2::geom_line(
        ggplot2::aes(y = .data$pressure_tag),
        color = "grey"
      ) +
      ggplot2::geom_point(
        data = pp[pp$label == "discard", ],
        ggplot2::aes(y = .data$pressure_tag),
        colour = "black"
      ) +
      ggplot2::geom_line(
        data = pp[pp$stapelev != "0|0", ],
        ggplot2::aes(y = .data$surface_pressure_norm, color = .data$stapelev)
      ) +
      ggplot2::geom_point(
        data = pp[pp$warning & pp$label != "discard" & pp$stapelev != "0|0", ],
        ggplot2::aes(y = .data$pressure_tag),
        fill = "orange", shape = 24, size = 3
      ) +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(name = "Pressure (hPa)") +
      ggplot2::theme(legend.position = "none")
  } else if (type %in% c("histogram", "hist")) {
    # Check if the empirical sd is greater than the sd used in the computation of the map

    # Remove error for flight
    pp <- pp[pp$stapelev != "0|0", ]

    pp$sd_param <- sd[pp$stap_id]
    pp$sd_ok <- pp$error_sd > pp$sd_param
    pp$stapelev <- factor(pp$stapelev, levels = tag_era5$stapelev)
    pp$warning_p <- pp$sd_param * warning_std_thr
    pp$warning_m <- -pp$sd_param * warning_std_thr

    lab <- ggplot2::as_labeller(stats::setNames(
      glue::glue("{tag_era5$stapelev} - SD: {tag_era5$error_sd}"),
      tag_era5$stapelev
    ))

    p <- ggplot2::ggplot(pp[pp$label != "discard", ], ggplot2::aes(x = .data$error)) +
      ggplot2::geom_histogram(
        ggplot2::aes(fill = .data$sd_ok),
        binwidth = .4
      ) +
      ggplot2::geom_vline(
        ggplot2::aes(xintercept = .data$warning_m),
        linetype = "dashed", colour = "red"
      ) +
      ggplot2::geom_vline(
        ggplot2::aes(xintercept = .data$warning_p),
        linetype = "dashed", colour = "red"
      ) +
      ggplot2::facet_wrap(~stapelev, scale = "free_y", labeller = lab) +
      ggplot2::scale_x_continuous(name = "Difference of pressure tag - ERA5 (hPa)") +
      ggplot2::scale_y_continuous(name = "Normalized histogram") +
      ggplot2::scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none", axis.text.y = ggplot2::element_blank())
  } else if (type %in% c("altitude", "alt")) {
    pp_alt <- pressurepath

    # find flight
    id_flight <- pp_alt$stap_id != round(pp_alt$stap_id)
    # group flight arbitrarily using the floor of their stap_id
    pp_alt$stap_id[id_flight] <- floor(pp_alt$stap_id)[id_flight]
    # convert to factor for ploting
    pp_alt$stap_id <- factor(pp_alt$stap_id)

    p <- ggplot2::ggplot() +
      ggplot2::geom_line(
        data = pp_alt,
        ggplot2::aes(
          x = .data$date,
          y = .data$altitude
        ),
        color = "grey",
        linewidth = 0.2
      ) +
      ggplot2::geom_line(
        data = pp_alt[!id_flight, ],
        ggplot2::aes(
          x = .data$date,
          y = .data$altitude,
          group = .data$stap_id,
          color = .data$stap_id
        )
      ) +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(name = "Altitude (m)") +
      ggplot2::theme(legend.position = "none")

    if (any(id_flight)) {
      p <- p +
        ggplot2::geom_line(
          data = pp_alt[id_flight, ],
          ggplot2::aes(x = .data$date, y = .data$altitude, group = .data$stap_id),
          color = "black"
        )
    }
  } else {
    cli::cli_abort("The type {.var {type}} of pressurepath plot does not exist. Available options
                   are: {.val {c('timeseries', 'histogram', 'altitude')}}")
  }

  if (plot_plotly) {
    return(plotly::ggplotly(p, dynamicTicks = TRUE))
  } else {
    return(p)
  }
}
