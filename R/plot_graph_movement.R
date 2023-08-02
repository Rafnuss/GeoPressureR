#' Plot pressure data of a `tag`
#'
#' This function display a plot of pressure timeseries recorded by a tag
#
#' @param graph A GeoPressureR graph object
#' @param speed Vector of speed value (km/h) used on the x-axis
#' @param plot_plotly Logical to use `plotly`
#' @export
plot_graph_movement <- function(graph,
                                speed = seq(1, 120),
                                plot_plotly = FALSE) {
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
