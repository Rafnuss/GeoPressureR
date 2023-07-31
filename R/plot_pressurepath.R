#' Plot pressure pressurepath
#'
#' description of check
#'
#' @param pressurepath rerer
#' @param type `"ts"` or `"hist"`
#' @param error_warning Threshold of pressure difference marking as ️warning (hPa)
#' @family pressurepath
#' @export
plot_pressurepath <- function(pressurepath,
                              type = "ts",
                              error_warning = \(err, error_sd, stap_id) (abs(err) / tag$param$sd[stap_id]) >= 3,
                              plot_plotly = TRUE) {

  assertthat::assert_that(is.data.frame(pressurepath))
  assertthat::assert_that(assertthat::has_name(
    pressurepath, c("date", "pressure_tag", "label", "stap_id", "pressure_era5", "altitude", "lat",
                    "lon", "pressure_era5_norm", "stap_ref")))

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
    stats::aggregate(list(error_sd = pp$error), list(stapelev = pp$stapelev), \(x) round(stats::sd(x, na.rm = T), 2)),
    stats::aggregate(list(error_offset = pp$pressure_era5 - pp$pressure_era5_norm), list(stapelev = pp$stapelev), \(x) round(mean(x), 2))
  )
  tag_era5 <- tag_era5[order(as.numeric(gsub("|",".", tag_era5$stapelev, fixed = T))),]

  # Add sd and offset to pp
  pp <- merge(pp, tag_era5)
  pp <- pp[order(pp$date), ]

  # knitr::kable(tag_era5, "simple")

  if ("ts" == type) {

    pp$warning <- error_warning(pp$error, pp$error_sd, pp$stap_id)

    # convert stapelev to factor for color
    pp$stap_id <- factor(pp$stap_id)

    p <- ggplot2::ggplot(pp, ggplot2::aes(x = date)) +
      ggplot2::geom_line(
        ggplot2::aes(y = pressure_tag),
        color = "grey"
      ) +
      ggplot2::geom_point(
        data = pp[pp$label == "discard", ],
        ggplot2::aes(y = pressure_tag),
        colour = "black"
      ) +
      ggplot2::geom_line(
        data = pp[pp$stap_id != 0, ],
        ggplot2::aes(y = pressure_era5_norm, color = stap_id)
      ) +
      ggplot2::geom_point(
        data = pp[pp$warning & pp$label != "discard", ],
        ggplot2::aes(y = pressure_tag),
        fill = "orange", shape = 24, size = 3
      ) +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(name = "Pressure (hPa)") +
      ggplot2::theme(legend.position = "none")
  } else if ("hist" == type) {
    # Check if the empirical sd is greater than the sd used in the computation of the map

    pp$sd_param <- tag$param$sd[pp$stap_id]
    pp$sd_ok <- pp$error_sd > tag$param$sd[pp$stap_id]

    pp$stapelev <- factor(pp$stapelev, levels = tag_era5$stapelev)

    lab <- ggplot2::as_labeller(stats::setNames(
      glue::glue("{tag_era5$stapelev} - SD: {tag_era5$error_sd}"),
      tag_era5$stapelev
    ))

    p <- ggplot2::ggplot(pp[pp$label == "", ], ggplot2::aes(x = error)) +
      ggplot2::geom_histogram(
        ggplot2::aes(
          y = (ggplot2::after_stat(count)) / tapply(ggplot2::after_stat(count), ggplot2::after_stat(PANEL), sum)[ggplot2::after_stat(PANEL)],
          fill = !sd_ok
        ),
        binwidth = .4
      ) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = sd_param * 3), linetype = "dashed", colour = "red") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = -sd_param * 3), linetype = "dashed", colour = "red") +
      ggplot2::facet_wrap(ggplot2::vars(stapelev), scale = "free", labeller = lab) +
      ggplot2::scale_x_continuous(name = "Difference of pressure tag - ERA5 (hPa)") +
      ggplot2::scale_y_continuous(name = "Normalized histogram") +
      scale_fill_manual(values = c("TRUE" = 'black', "FALSE" = 'red')) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none", axis.text.y = element_blank())


  } else if (type == "altitude") {
    p <- ggplot2::ggplot(pp, ggplot2::aes(x = date)) +
      ggplot2::geom_line(
        ggplot2::aes(y = altitude),
        color = "grey"
      ) +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(name = "Altitude (m)") +
      ggplot2::theme(legend.position = "none")
  }

  if (plot_plotly) {
    return(plotly::ggplotly(p, dynamicTicks = TRUE))
  } else {
    return(p)
  }
}
