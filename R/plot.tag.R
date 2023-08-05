#' Plot `tag`
#'
#' This function display the basic information on a tag list
#'
#' @param x A GeoPressureR `tag` object.
#' @param type type of the plot to display. One of "pressure", "acceleration", "light", "twilight"
#' "map", "map_pressure", "map_light", "map_pressure_mse", "map_pressure_mask", "mask_water"
#' @inheritParams leaflet::colorNumeric
#' @param ... Additional parameters
#'
#' @return `tag` is returned invisibly and unchanged
#' @family tag
#' @export
plot.tag <- function(x, type = NULL, palette = NULL, ...) {
  tag <- x

  # Define default
  if (is.null(type)) {
    status <- tag_status(tag)
    if ("map_pressure" %in% status) {
      type <- "map"
    } else {
      type <- "pressure"
    }
  }

  if (type == "pressure") {
    plot_tag_pressure(tag, ...)
  } else if (type == "acceleration") {
    plot_tag_acceleration(tag, ...)
  } else if (type == "light") {
    plot_tag_light(tag, ...)
  } else if (type == "twilight") {
    plot_tag_twilight(tag, ...)
  } else if (grepl("map", type)) {
    # Define optimal color palete base on the type of variable shown
    if (is.null(palette)) {
      if ("map_pressure" == type) {
        palette <- "PuBu"
      } else if ("map_light" == type) {
        palette <- "YlOrBr"
      } else if ("map_pressure_mse" == type) {
        palette <- "BuPu"
      } else if ("map_pressure_mask" == type) {
        palette <- "PuRd"
      } else if ("mask_water" == type) {
        palette <- "Greys"
      } else {
        palette <- "OrRd"
      }
    }
    # Accept type="map" for default map determined by `tag2map` with likelihood = NA
    if (type == "map") {
      type <- NULL
    }

    # Retrieve the map
    map <- tag2map(tag, likelihood = type)

    # plot the map
    plot.map(map, stap = tag$stap, palette = palette, ...)
  } else {
    cli::cli_abort(c(
      "x" = "The type {.val {type}} is not known",
      ">" = "{.var type} should be one of {.val {c('pressure', 'acceleration', 'light', 'twilight',
      'map', 'map_pressure', 'map_light', 'map_pressure_mse', 'map_pressure_mask', 'mask_water')}}"
    ))
  }
}

#' Plot pressure data of a `tag`
#'
#' This function display a plot of pressure timeseries recorded by a tag
#
#' @param tag A GeoPressureR `tag` object
#' @param plot_plotly Logical to use `plotly`
#' @param quiet Logical to hide warning message about labeling
#' @param warning_stap_length Threshold number of pressure datapoints flagged as ️warning (hourly.
#' @param warning_pressure_diff Threshold of pressure hourly difference marking as ️warning (hPa)
#' @export
plot_tag_pressure <- function(tag,
                              plot_plotly = TRUE,
                              quiet = FALSE,
                              warning_pressure_diff = 3,
                              warning_stap_length = 12) {
  tag_assert(tag)
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = tag$pressure,
      ggplot2::aes_string(x = "date", y = "value"),
      color = "grey"
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(name = "Pressure (hPa)") +
    ggplot2::theme(legend.position = "none")

  # Only if tag is labeled
  if ("stap" %in% names(tag)) {
    # compute the pressure at the hourly scale
    pres <- geopressure_map_preprocess(tag)

    # extract stap for convenience
    stap <- tag$stap

    # convert stapelev to factor for color
    pres$stapelev <- factor(pres$stapelev)

    # Compute number of datapoint per stationary period
    pressure_length <- merge(stap[stap$include & is.na(stap$known_lat), ],
                             data.frame(table(pres$stap_id)),
                             by.x = "stap_id", by.y = "Var1", all.x = TRUE
    )
    pressure_length$Freq[is.na(pressure_length$Freq)] <- 0

    id_length <- which(pressure_length$Freq <= warning_stap_length)
    if (!quiet) {
      cli::cli_h3("Pre-processed pressure data length")
      if (length(id_length) > 0) {
        for (i in seq_len(length(id_length))) {
          cli::cli_alert_warning("There are only {.val {pressure_length$Freq[id_length[i]]}} \\
            datapoint{?s} for the stationary period {.val {pressure_length$stap_id[id_length[i]]}}")
        }
      } else {
        cli::cli_alert_success("All stationary periods have more than \\
                              {.val {warning_stap_length}} datapoints.")
      }
    }

    # Pressure difference
    id_diff_1hr <-
      pres_diff <- data.frame(
        value = abs(diff(pres$value)),
        value_avg = utils::head(pres$value, -1) + diff(pres$value) / 2,
        date = utils::head(pres$date, -1) + diff(pres$date) / 2,
        stap_id = (utils::tail(pres$stap_id, -1) + utils::head(pres$stap_id, -1)) / 2
      )
    # Only keep the 1 hours difference
    pres_diff <- pres_diff[as.numeric(diff(pres$date), units = "hours") == 1, ]
    # Remove diff overlapping between stationary periods/flight
    pres_diff <- pres_diff[(pres_diff$stap_id %% 1) == 0 & pres_diff$stap_id != 0, ]
    # Only keep difference which are above warning limit
    pres_diff <- pres_diff[pres_diff$value >= warning_pressure_diff, ]
    # Sort data.frame for displaying top 10 max
    pres_diff <- pres_diff[order(pres_diff$value, decreasing = TRUE), ]

    pressure_diff_max_display <- 10

    if (!quiet) {
      cli::cli_h3("Pressure difference")
      if (nrow(pres_diff) > 0) {
        cli::cli_alert("{.val {nrow(pres_diff)}} timestamp{?s} show{?s/} abnormal hourly change \\
                            in pressure (i.e., >{.val {warning_pressure_diff}}hPa): ")
        for (i in seq_len(min(nrow(pres_diff), pressure_diff_max_display))) {
          cli::cli_alert_warning("{pres_diff$date[i]} | stap: {pres_diff$stap_id[i]} | \\
                                  {.val {round(pres_diff$value[i],1)}} hPa ")
        }
        if (nrow(pres_diff) > pressure_diff_max_display) {
          cli::cli_alert("{.val {nrow(pres_diff)-pressure_diff_max_display}} more \\
                             timestamp{?s} {?is/are} exceeding the threshold.")
        }
      } else {
        cli::cli_alert_success("All hourly changes in pressure are below \\
                               {.val {warning_pressure_diff}} hPa.")
      }
    }

    p <- p +
      ggplot2::geom_point(
        data = tag$pressure[tag$pressure$label == "discard", ],
        ggplot2::aes_string(x = "date", y = "value"),
        colour = "black"
      ) +
      ggplot2::geom_line(
        data = pres,
        ggplot2::aes_string(x = "date", y = "value", color = "stapelev")
      ) +
      ggplot2::geom_point(
        data = pres_diff,
        ggplot2::aes_string(x = "date", y = "value_avg"),
        fill = "orange", shape = 24, size = 2
      )
  }

  if (plot_plotly) {
    return(plotly::ggplotly(p, dynamicTicks = TRUE))
  } else {
    return(p)
  }
}

#' Plot acceleration data of a `tag`
#'
#' This function display a plot of acceleration timeseries recorded by a tag
#'
#' @param tag A GeoPressureR `tag` object
#' @param plot_plotly Logical to use `plotly`
#' @param label_auto Logical to compute and plot the flight label using `tag_label_auto()`. Only if
#' labels are not already present on tag$acceleration$label
#' @inheritParams tag_label_auto
#' @export
plot_tag_acceleration <- function(tag,
                                  plot_plotly = TRUE,
                                  label_auto = TRUE,
                                  min_duration = 30) {
  tag_assert(tag)
  assertthat::assert_that(assertthat::has_name(tag, "acceleration"))

  # If not label, use default auto_label
  if (!("label" %in% names(tag$acceleration)) & label_auto) {
    tag <- tag_label_auto(tag, min_duration = min_duration)
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = tag$acceleration,
      ggplot2::aes_string(x = "date", y = "value"),
      color = "black"
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(name = "Acceleration") +
    ggplot2::theme(legend.position = "none")

  if ("label" %in% names(tag$acceleration)) {
    p <- p +
      ggplot2::geom_point(
        data = tag$acceleration[tag$acceleration$label == "flight", ],
        ggplot2::aes_string(x = "date", y = "value"),
        fill = "red", shape = 23, size = 2,
      )
  }

  if (plot_plotly) {
    return(plotly::ggplotly(p, dynamicTicks = TRUE))
  } else {
    return(p)
  }
}


#' Plot light data of a `tag`
#'
#' This function display a plot of light timeseries recorded by a tag
#'
#' @param tag A GeoPressureR `tag` object
#' @param plot_plotly Logical to use `plotly`
#' @param transform_light Logical to display a log transformation of light
#' @export
plot_tag_light <- function(tag,
                           transform_light = TRUE,
                           plot_plotly = TRUE) {
  tag_assert(tag)
  assertthat::assert_that(assertthat::has_name(tag, "light"))

  l <- tag$light
  if (transform_light) {
    l$value <- log(l$value + 0.0001) + abs(min(log(l$value + 0.0001)))
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = l,
      ggplot2::aes_string(x = "date", y = "value"),
      color = "grey"
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(name = "Light") +
    ggplot2::theme(legend.position = "none")

  # Only if twilight are already computed
  if ("twilight" %in% names(tag)) {
    twl <- tag$twilight
    twl$datetime <- twl$twilight
    twl$twilight <- ifelse(twl$rise, "sunset", "sunrise")

    p <- p +
      ggplot2::geom_vline(
        data = twl,
        ggplot2::aes_string(xintercept = "datetime", color = "twilight")
      ) +
      ggplot2::scale_color_manual(values = c("sunrise" = "#FFD700", "sunset" = "#FF4500"))
  }

  if (plot_plotly) {
    return(plotly::ggplotly(p, dynamicTicks = TRUE))
  } else {
    return(p)
  }
}

#' Plot twilight data of a `tag`
#'
#' This function display a plot of twilight timeseries recorded by a tag
#'
#' @param tag A GeoPressureR `tag` object
#' @param plot_plotly Logical to use `plotly`
#' @param transform_light Logical to display a log transformation of light
#' @export
plot_tag_twilight <- function(tag,
                              transform_light = TRUE,
                              plot_plotly = FALSE) {
  tag_assert(tag, "twilight")

  l <- tag$light
  if (transform_light) {
    l$value <- log(l$value + 0.0001) + abs(min(log(l$value + 0.0001)))
  }

  # Compute the matrix representation of light
  mat <- light2mat(l, twl_offset = tag$param$twl_offset)

  # Convert to long format data.fram to be able to plot with ggplot
  df <- as.data.frame(mat$value)
  names(df) <- mat$day
  df$time <- factor(mat$time, levels = mat$time)
  df_long <- stats::reshape(df,
                     direction = "long",
                     varying = list(utils::head(names(df), -1)),
                     v.names = "light",
                     idvar = "time",
                     timevar = "date",
                     times = utils::head(names(df), -1)
  )
  df_long$date <- as.Date(df_long$date)

  p <- ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = df_long,
      ggplot2::aes_string(x = "date", y = "time", fill = "light")
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_gradient(low = "black", high = "white") +
    ggplot2::scale_x_date(name = "Date")

  if ("twilight" %in% names(tag)) {
    twl <- tag$twilight
    twl$date <- as.Date(twl$twilight)
    twl$time <- format(twl$twilight, "%H:%M")
    twl$stap_id <- factor(round(twl$stap_id))
    if ("label" %in% names(twl)) {
      twl$discard <- twl$label == "discard"
    } else {
      twl$discard <- FALSE
    }

    # plotly doesn't like much changing color...
    if (plot_plotly) {
      p <- p +
        ggplot2::geom_point(
          data = twl,
          ggplot2::aes_string(x = "date", y = "time"),
          colour = "red",
          size = 2,
          shape = 16
        )
    } else {
      p <- p +
        ggplot2::geom_point(
          data = twl,
          ggplot2::aes_string(x = "date", y = "time", colour = "stap_id"),
          size = 2,
          shape = 16
        )
    }
    p <- p +
      ggplot2::geom_point(
        data = twl[twl$discard, ],
        ggplot2::aes_string(x = "date", y = "time"),
        size = 3,
        shape = 4,
        stroke = 2,
        colour = "yellow"
      )
  }

  if (plot_plotly) {
    return(plotly::ggplotly(p + ggplot2::scale_y_discrete(name = "Time"), dynamicTicks = TRUE))
  } else {
    # Setting the breaks seems to mess up plotly
    p <- p +
      ggplot2::scale_y_discrete(name = "Time", breaks = format(seq(as.POSIXct("2015-1-1 0:00"), as.POSIXct("2015-1-1 23:00"), by = "hour"), "%H:%M"))
    return(p)
  }
}
