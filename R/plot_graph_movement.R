#' Plot pressure data of a `tag`
#'
#' This function display a plot of pressure timeseries recorded by a tag
#
#' @param graph graph constructed with [`graph_create()`]
#' @inheritParams plot.tag
#' @param stap_length_danger Threshold number of pressure datapoints flagged as ️danger (hourly).
#' @param stap_length_warning Threshold number of pressure datapoints flagged as ️warning (hourly).
#' @param pressure_diff_danger Threshold of pressure hourly difference marking as ️danger (hPa)
#' @param pressure_diff_warning Threshold of pressure hourly difference marking as ️warning (hPa)
#' @export
plot_graph_movement <- function(graph,
                                speed = seq(1, 120),
                                plot_plotly = FALSE,
                                ...) {
  # Check that graph is correct
  graph_assert(graph, "movement")

  d <- data.frame(
    speed = speed,
    prob = speed2prob(speed, graph$movement)
    )
  lsf <- data.frame(low_speed_fix = graph$movement$low_speed_fix)

  if (assertthat::has_name(graph, c("ws"))) {
    xlab <- "Airspeed [km/h]"
  } else {
    xlab <- "Groundspeed [km/h]"
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = d,
      ggplot2::aes_string(x = "speed", y = "prob"),
      color = "grey"
    ) +
    ggplot2::geom_vline(
      data = lsf,
      ggplot2::aes_string(xintercept = "low_speed_fix"),
      color = "red"
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(name = "Probability") +
    ggplot2::scale_x_continuous(name = xlab) +
    ggplot2::theme(legend.position = "none")

  if (plot_plotly) {
    return(plotly::ggplotly(p, dynamicTicks = TRUE))
  } else {
    return(p)
  }
}
