#' Check pressure data
#'
#' @description
#' This function perform the following basic check on the pressure data and its suitability for
#' [`geopressure_map()`]:
#' - Is there sufficient data for each stationary periods?
#' - Is the hourly change of pressure within each stationary period possible for a natural variation
#' of pressure or does it suggest vertical movement of the bird (flight or elevation change)?
#'
#' Beyond the hood, it uses [`geopressure_map_preprocess()`] to compute the cleaned
#' pressure to be match and perform the check on those (an not the raw pressure data).
#'
#' Use `known` and a `stap_include` to only check the pressure belonging to a stationary period
#' which is included and not known.
#'
#' @return Return `TRUE` if no danger were raised (warning is ok).
#' @inheritParams geostap_create
#' @param stap_length_danger Threshold number of pressure datapoints flagged as ️danger (hourly).
#' @param stap_length_warning Threshold number of pressure datapoints flagged as ️warning (hourly).
#' @param pressure_diff_danger Threshold of pressure hourly difference marking as ️danger (hPa)
#' @param pressure_diff_warning Threshold of pressure hourly difference marking as ️warning (hPa)
#' @export
plot_pressure_tag <- function(tag,
                              known = data.frame(
                                stap_id = integer(),
                                known_lat = double(),
                                known_lon = double()
                              ),
                              stap_include = tag$stap$stap_id,
                              pressure_diff_danger = 5,
                              pressure_diff_warning = 3,
                              stap_length_danger = 6,
                              stap_length_warning = 12) {
  # Create a "fake" geostap
  geostap <- geostap_create(
    tag = tag,
    extent = c(-180, 180, -90, 90),
    known = known,
    stap_include = stap_include
  )

  # extract stap for convininece
  stap <- geostap$stap

  # compute the pressure at the hourly scale
  pres <- geopressure_map_preprocess(tag$pressure, stap)

  # convert stapelev to factor for color
  pres$stapelev <- factor(pres$stapelev)

  # Compute number of datapoint per stationary period
  pressure_length <- merge(stap[stap$include & is.na(stap$known_lat), ],
    data.frame(table(pres$stap_id)),
    by.x = "stap_id", by.y = "Var1", all.x = TRUE
  )
  pressure_length$Freq[is.na(pressure_length$Freq)] <- 0

  id_length <- pressure_length$stap_id[pressure_length$Freq <= stap_length_warning]
  if (length(id_length) > 0) {
    for (i in seq_len(length(id_length))) {
      if (pressure_length$Freq[id_length[i]] <= stap_length_danger) {
        cli::cli_alert_danger("There are only {pressure_length$Freq[id_length[i]]} \\
        datapoint{?s} for stationary period {pressure_length$stap_id[id_length[i]]}")
      } else {
        cli::cli_alert_warning("There are only {pressure_length$Freq[id_length[i]]} \\
        datapoint{?s} for stationary period {pressure_length$stap_id[id_length[i]]}")
      }
    }
  } else {
    cli::cli_alert_success("All stationary periods have more than {stap_length_warning} \\
                           datapoints.")
  }

  # cli::cli_rule("Pressure difference")
  pres_diff <- abs(diff(pres$value))
  pres_diff[diff(pres$stap_id) > 0] <- 0
  pres_diff[utils::tail(pres$stap_id, -1) == 0 |
    utils::head(pres$stap_id, -1) == 0] <- 0
  pres$diff_status <- ""
  pres$diff_status[c(pres_diff >= pressure_diff_warning, FALSE)] <- "warning"
  pres$diff_status[c(pres_diff >= pressure_diff_danger, FALSE)] <- "danger"
  pres$diff_status <- factor(pres$diff_status)

  pressure_diff_max_display <- 10
  id_diff <- which(pres_diff >= pressure_diff_warning)
  if (any(pres$diff_status != "")) {
    id_diff <- id_diff[order(pres_diff[id_diff], decreasing = TRUE)]
    cli::cli_alert_info("The following {.val {length(id_diff)}} timestamps show hourly change in \\
                        pressure out of the ordinary (i.e., {.val >{pressure_diff_warning}} hPa).")
    pressure_diff_exceed <- length(id_diff) > pressure_diff_max_display
    id_diff <- id_diff[seq_len(min(length(id_diff), pressure_diff_max_display))]
    for (i in seq_len(length(id_diff))) {
      if (pres_diff[id_diff[i]] >= pressure_diff_danger) {
        cli::cli_alert_danger("{pres$date[id_diff[i]]} | stap: \\
                              {pres$stap_id[id_diff[i]]} | \\
                              {.val {round(pres_diff[id_diff[i]],1)}} hPa ")
      } else {
        cli::cli_alert_warning("{pres$date[id_diff[i]]} | stap: \\
                               {pres$stap_id[id_diff[i]]} | \\
                               {.val {round(pres_diff[id_diff[i]],1)}} hPa")
      }
    }
    if (pressure_diff_exceed) {
      cli::cli_alert_info("We only display the first {pressure_diff_max_display} timestamp sorted \\
                        by decreasing value.")
    }
  } else {
    cli::cli_alert_success("All hourly change in pressure are below \\
                           {.val {pressure_diff_warning}} hPa.")
  }


  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = tag$pressure,
      ggplot2::aes(x = date, y = value),
      color = "grey"
    ) +
    ggplot2::geom_line(
      data = pres,
      ggplot2::aes(x = date, y = value, color = stapelev)
    ) +
    ggplot2::geom_point(
      data = tag$pressure[tag$pressure$label == "discard", ],
      ggplot2::aes(x = date, y = value),
      colour = "black"
    ) +
    ggplot2::geom_point(
      data = pres[pres$diff_status == "danger", ],
      ggplot2::aes(x = date, y = value),
      fill = "red", shape = 23, size = 2
    ) +
    ggplot2::geom_point(
      data = pres[pres$diff_status == "warning", ],
      ggplot2::aes(x = date, y = value),
      fill = "orange", shape = 24, size = 2
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(name = "Pressure (hPa)") +
    ggplot2::theme(legend.position = "none")

  plotly::ggplotly(p, dynamicTicks = TRUE)
}
