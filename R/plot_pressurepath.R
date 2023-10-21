#' Plot a `pressurepath`
#'
#' Display a `pressurepath` data.frame as a timeseries or histogram
#'
#' @param pressurepath a GeoPressureR `pressurepath` data.frame.
#' @param type Timeseries `"ts"` or histogram `"hist"`
#' @inheritParams geopressure_map
#' @param warning_std_thr Threshold of outliar, coefficient of the [z-score](
#' https://en.wikipedia.org/wiki/Standard_score)
#' @param plot_plotly logical to use `plotly`
#'
#' @return a plot or ggplotly object.
#'
#' @examples
#' setwd(system.file("extdata", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = TRUE) |> tag_label(quiet = TRUE)
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
#' @family pressurepath
#' @export
plot_pressurepath <- function(pressurepath,
                              type = "ts",
                              sd = attr(pressurepath, "sd"),
                              warning_std_thr = 3,
                              plot_plotly = TRUE) {
  assertthat::assert_that(is.data.frame(pressurepath))
  assertthat::assert_that(assertthat::has_name(
    pressurepath, c(
      "date", "pressure_tag", "label", "stap_id", "pressure_era5", "altitude", "lat", "lon",
      "pressure_era5_norm", "stap_ref"
    )
  ))

  if (is.null(sd)) {
    sd <- rep(1, times = length(unique(pressurepath$stap_id)))
  }

  pp <- pressurepath

  # Compute the error
  pp$error <- pp$pressure_tag - pp$pressure_era5_norm
  pp$error[pp$label != ""] <- NA

  # Group by stapelev rather than stap in order to assess the use of elev
  pp$stapelev <- paste(pp$stap_id,
    ifelse(startsWith(pp$label, "elev_"), gsub("^.*?elev_", "", pp$label), "0"),
    sep = "|"
  )

  # Compute the error std and offset
  tag_era5 <- merge(
    stats::aggregate(
      list(error_sd = pp$error), list(stapelev = pp$stapelev),
      \(x) round(stats::sd(x, na.rm = TRUE), 2)
    ),
    stats::aggregate(
      list(error_offset = pp$pressure_era5 - pp$pressure_era5_norm),
      list(stapelev = pp$stapelev), \(x) round(mean(x), 2)
    )
  )
  tag_era5 <- tag_era5[order(as.numeric(gsub("|", ".", tag_era5$stapelev, fixed = TRUE))), ]

  # Add sd and offset to pp
  pp <- merge(pp, tag_era5)
  pp <- pp[order(pp$date), ]

  # knitr::kable(tag_era5, "simple")

  if ("ts" == type) {
    pp$warning <- (abs(pp$error) / sd[ifelse(pp$stap_id == 0, 1, pp$stap_id)]) >= warning_std_thr

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
        data = pp[pp$stap_id != 0, ],
        ggplot2::aes(y = .data$pressure_era5_norm, color = .data$stap_id)
      ) +
      ggplot2::geom_point(
        data = pp[pp$warning & pp$label != "discard" & pp$stap_id != 0, ],
        ggplot2::aes(y = .data$pressure_tag),
        fill = "orange", shape = 24, size = 3
      ) +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(name = "Pressure (hPa)") +
      ggplot2::theme(legend.position = "none")
  } else if ("hist" == type) {
    # Check if the empirical sd is greater than the sd used in the computation of the map

    pp$sd_param <- sd[ifelse(pp$stap_id == 0, 1, pp$stap_id)]
    pp$sd_ok <- pp$error_sd > pp$sd_param
    pp$stapelev <- factor(pp$stapelev, levels = tag_era5$stapelev)
    pp$warning_p <- pp$sd_param * warning_std_thr
    pp$warning_m <- -pp$sd_param * warning_std_thr

    lab <- ggplot2::as_labeller(stats::setNames(
      glue::glue("{tag_era5$stapelev} - SD: {tag_era5$error_sd}"),
      tag_era5$stapelev
    ))

    p <- ggplot2::ggplot(pp[pp$label == "", ], ggplot2::aes(x = .data$error)) +
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
  } else if (type == "altitude") {
    pp_alt <- pressurepath2altitude(pp)
    pp_alt$stap_id <- factor(pp_alt$stap_id)

    p <- ggplot2::ggplot() +
      ggplot2::geom_line(
        data = pp_alt[pp_alt$stap_id != 0, ],
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

    if (sum(pp_alt$stap_id == 0) > 0) {
      p <- p +
        ggplot2::geom_line(
          data = pp_alt[pp_alt$stap_id == 0, ],
          ggplot2::aes(x = .data$date, y = .data$altitude, group = .data$stap_s),
          color = "black"
        )
    }
  }

  if (plot_plotly) {
    return(plotly::ggplotly(p, dynamicTicks = TRUE))
  } else {
    return(p)
  }
}
