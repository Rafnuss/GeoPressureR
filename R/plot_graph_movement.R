#' Plot movement model of a `graph`
#'
#' This function display a plot of pressure time series recorded by a tag
#
#' @param graph a GeoPressureR `graph` object.
#' @param speed Vector of speed value (km/h) used on the x-axis.
#' @param plot_plotly logical to use `plotly`
#'
#' @return a plot or ggplotly object.
#'
#' @family movement
#' @export
plot_graph_movement <- function(graph,
                                speed = seq(0, 120),
                                plot_plotly = FALSE) {
  # Check that graph is correct
  graph_assert(graph, "movement")

  d <- data.frame(
    speed = speed,
    prob = speed2prob(speed, graph$param$graph_set_movement)
  )
  lsf <- data.frame(low_speed_fix = graph$param$graph_set_movement$low_speed_fix)

  if (graph$param$graph_set_movement$type == "as") {
    xlab <- "Airspeed [km/h]"
  } else {
    xlab <- "Groundspeed [km/h]"
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = d,
      ggplot2::aes(x = .data$speed, y = .data$prob),
      color = "grey"
    ) +
    ggplot2::geom_vline(
      data = lsf,
      ggplot2::aes(xintercept = .data$low_speed_fix),
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
