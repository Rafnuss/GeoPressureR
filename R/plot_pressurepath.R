#' Plot pressure pressurepath
#'
#' description of check
#'
#' @param pressurepath rerer
#' @param type `"ts"` or `"hist"`
#' @param pressure_err_warning Threshold of pressure difference marking as ️warning (hPa)
#' @param pressure_err_danger Threshold of pressure difference marking as ️danger (hPa)
#' @export
plot_pressurepath <- function(pressurepath,
                               type = "ts",
                               pressure_err_warning = 2,
                               pressure_err_danger = 5) {
  # Compute the error
  pressurepath$pressure_err <- pressurepath$pressure_tag - pressurepath$pressure_era5_norm

  # Group by stapelev rather tha stap in order to assess the use of elev
  pressurepath$stapelev <- paste(pressurepath$stap_id,
    ifelse(startsWith(pressurepath$label, "elev_"),
      gsub("^.*?elev_", "", pressurepath$label),
      "0"
    ),
    sep = "|"
  )

  # Compute the error std and offset
  tag_era5 <- merge(
    stats::aggregate(list(error_sd = pressurepath$pressure_err), list(stap_id = pressurepath$stapelev), \(x) round(stats::sd(x), 2)),
    stats::aggregate(list(offset = pressurepath$pressure_era5 - pressurepath$pressure_era5_norm), list(stap_id = pressurepath$stapelev), \(x) round(mean(x), 2))
  )

  knitr::kable(tag_era5, "simple")

  if ("ts" == type) {
    pressurepath$err_status <- ""
    pressurepath$err_status[abs(pressurepath$pressure_err) >= pressure_err_warning] <- "warning"
    pressurepath$err_status[abs(pressurepath$pressure_err) >= pressure_err_danger] <- "danger"
    pressurepath$err_status[pressurepath$label == "discard"] <- ""
    pressurepath$err_status <- factor(pressurepath$err_status)

    # convert stapelev to factor for color
    pressurepath$stap_id <- factor(pressurepath$stap_id)

    p <- ggplot2::ggplot(pressurepath, ggplot2::aes(x = date)) +
      ggplot2::geom_line(
        ggplot2::aes(y = pressure_tag, group = stap_id),
        color = "grey"
      ) +
      ggplot2::geom_point(
        data = pressurepath[pressurepath$label == "discard", ],
        ggplot2::aes(y = pressure_tag),
        colour = "black"
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = pressure_era5_norm, color = stap_id)
      ) +
      ggplot2::geom_point(
        data = pressurepath[pressurepath$err_status == "danger", ],
        ggplot2::aes(y = pressure_tag),
        fill = "red", shape = 23, size = 3
      ) +
      ggplot2::geom_point(
        data = pressurepath[pressurepath$err_status == "warning", ],
        ggplot2::aes(y = pressure_tag),
        fill = "orange", shape = 24, size = 3
      ) +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(name = "Pressure (hPa)") +
      ggplot2::theme(legend.position = "none")

    plotly::ggplotly(p, dynamicTicks = TRUE)
  }

  if ("hist" == type) {
    lab <- ggplot2::as_labeller(stats::setNames(
      glue::glue("{tag_era5$stap_id} - SD: {tag_era5$error_sd}"),
      tag_era5$stap_id
    ))

    ggplot2::ggplot(pressurepath, ggplot2::aes(x = pressure_err)) +
      ggplot2::geom_histogram(ggplot2::aes(y = (ggplot2::after_stat(count)) / tapply(ggplot2::after_stat(count), ..PANEL.., sum)[..PANEL..]), binwidth = .4) +
      ggplot2::facet_wrap(ggplot2::vars(stapelev), scale = "free", labeller = lab) +
      ggplot2::scale_x_continuous(name = "Difference of pressure tag - ERA5 (hPa)") +
      ggplot2::scale_y_continuous(name = "Normalized histogram") +
      ggplot2::theme_bw()
  }
}
