#' Plot pressure path_pres
#'
#' description of check
#'
#' @param path_pres rerer
#' @param type `"ts"` or `"hist"`
#' @param pressure_err_warning Threshold of pressure difference marking as ️warning (hPa)
#' @param pressure_err_danger Threshold of pressure difference marking as ️danger (hPa)
#' @export
plot_pressure_path <- function(path_pres,
                               type = "ts",
                               pressure_err_warning = 2,
                               pressure_err_danger = 5) {
  # Compute the error
  path_pres$pressure_err <- path_pres$pressure_tag - path_pres$pressure_era5_norm

  # Group by stapelev rather tha stap in order to assess the use of elev
  path_pres$stapelev <- paste(path_pres$stap_id,
    ifelse(startsWith(path_pres$label, "elev_"),
      gsub("^.*?elev_", "", path_pres$label),
      "0"
    ),
    sep = "|"
  )

  # Compute the error std and offset
  tag_era5 <- merge(
    stats::aggregate(list(error_sd = path_pres$pressure_err), list(stap_id = path_pres$stapelev), \(x) round(stats::sd(x), 2)),
    stats::aggregate(list(offset = path_pres$pressure_era5 - path_pres$pressure_era5_norm), list(stap_id = path_pres$stapelev), \(x) round(mean(x), 2))
  )

  knitr::kable(tag_era5, "simple")

  if ("ts" == type) {
    path_pres$err_status <- ""
    path_pres$err_status[abs(path_pres$pressure_err) >= pressure_err_warning] <- "warning"
    path_pres$err_status[abs(path_pres$pressure_err) >= pressure_err_danger] <- "danger"
    path_pres$err_status[path_pres$label == "discard"] <- ""
    path_pres$err_status <- factor(path_pres$err_status)

    # convert stapelev to factor for color
    path_pres$stap_id <- factor(path_pres$stap_id)

    p <- ggplot2::ggplot(path_pres, ggplot2::aes(x = date)) +
      ggplot2::geom_line(
        ggplot2::aes(y = pressure_tag, group = stap_id),
        color = "grey"
      ) +
      ggplot2::geom_point(
        data = path_pres[path_pres$label == "discard", ],
        ggplot2::aes(y = pressure_tag),
        colour = "black"
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = pressure_era5_norm, color = stap_id)
      ) +
      ggplot2::geom_point(
        data = path_pres[path_pres$err_status == "danger", ],
        ggplot2::aes(y = pressure_tag),
        fill = "red", shape = 23, size = 3
      ) +
      ggplot2::geom_point(
        data = path_pres[path_pres$err_status == "warning", ],
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

    ggplot2::ggplot(path_pres, ggplot2::aes(x = pressure_err)) +
      ggplot2::geom_histogram(ggplot2::aes(y = (ggplot2::after_stat(count)) / tapply(ggplot2::after_stat(count), ..PANEL.., sum)[..PANEL..]), binwidth = .4) +
      ggplot2::facet_wrap(ggplot2::vars(stapelev), scale = "free", labeller = lab) +
      ggplot2::scale_x_continuous(name = "Difference of pressure tag - ERA5 (hPa)") +
      ggplot2::scale_y_continuous(name = "Normalized histogram") +
      ggplot2::theme_bw()
  }
}
