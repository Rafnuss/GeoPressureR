#' Plot a `tag` object
#'
#' @description
#' This function plot a GeoPressureR `tag` object as a time series or a map.
#'
#' By default, `type` is determined in the following order of preference according to availability:
#' `c("map_pressure", "map_light")`, `"map_pressure"`, `"map_light"`, `"pressure"`.
#'
#' `plot.tag()` calls different plotting functions depending on `type`.
#' - `"pressure"`: `plot_tag_pressure()`
#' - `"light"`: `plot_tag_light()`
#' - `"acceleration"`: `plot_tag_acceleration()`
#' - `"temperature"`: `plot_tag_temperature()`
#' - `"twilight"`: `plot_tag_twilight()`
#' - `"map_*"` : `plot.map()` with `tag$map_*` as first argument.
#'
#' Refers to these functions for additional parameters and more flexibility in the plotting.
#'
#' @param x a GeoPressureR `tag` object.
#' @param type type of the plot to display. One of `"pressure"`, `"acceleration"`, `"light"`,
#' `"temperature"`, `"twilight"`, `"map"`, `"map_pressure"`, `"map_light"`, `"map_pressure_mse"`,
#' `"map_pressure_mask"`, `"mask_water"`. Map can be combined by providing a vector of type.
#' @param ... additional parameters for `plot_tag_pressure()`, `plot_tag_acceleration()`,
#' `plot_tag_light()`, `plot_tag_twilight()` or `plot.map()`
#'
#' @return a plot, ggplotly or leaflet object.
#'
#' @examples
#' owd <- setwd(system.file("extdata", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = TRUE) |>
#'   tag_label(quiet = TRUE) |>
#'   twilight_create() |>
#'   twilight_label_read()
#' setwd(owd)
#'
#' # By default, plot will display the time series of pressure
#' plot(tag)
#' # Change the `type` to display other sensor
#' plot(tag, type = "acceleration")
#' plot(tag, type = "light")
#' # Twilight is display as an image
#' plot(tag, type = "twilight")
#'
#' # After you compute any likelihood map, the default will
#' # become this map (i.e., `type = "map"`)
#' tag <- tag_set_map(tag,
#'   extent = c(-16, 23, 0, 50),
#'   scale = 4,
#'   known = data.frame(
#'     stap_id = 1,
#'     known_lon = 17.05,
#'     known_lat = 48.9
#'   )
#' ) |>
#'   geopressure_map(quiet = TRUE)
#' plot(tag)
#' # The likelihood map of light can be display with
#' tag <- geolight_map(tag, quiet = TRUE)
#' plot(tag, type = "map_light")
#' # When both pressure and light likelihood are present,
#' # the default is to display their products, equivalent
#' # to choose `type = c("map_pressure", "map_light")`
#' plot(tag)
#' @family tag plot_tag
#'
#' @export
plot.tag <- function(x, type = NULL, ...) {
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
  } else if (type == "temperature") {
    plot_tag_temperature(tag, ...)
  } else if (type == "twilight") {
    plot_tag_twilight(tag, ...)
  } else if (grepl("map", type)) {
    # Define optimal color palette based on the type of variable shown

    # Accept type="map" for default map determined by `tag2map` with likelihood = NA
    if (type == "map") {
      type <- NULL
    }

    # Retrieve the map
    map <- tag2map(tag, likelihood = type)

    # plot the map
    plot.map(map, ...)
  } else {
    cli::cli_abort(c(
      "x" = "The type {.val {type}} is not known",
      ">" = "{.var type} should be one of {.val {c('pressure', 'acceleration', 'light',
      'temperature', twilight', 'map', 'map_pressure', 'map_light', 'map_pressure_mse',
      'map_pressure_mask', 'mask_water')}}"
    ))
  }
}

#' Plot pressure data of a `tag`
#'
#' This function display a plot of pressure time series recorded by a tag
#
#' @param tag a GeoPressureR `tag` object.
#' @param plot_plotly logical to use `plotly`.
#' @param quiet logical to hide warning message about label.
#' @param warning_stap_length Threshold number of pressure datapoints flagged as ️warning (hourly.
#' @param warning_pressure_diff Threshold of pressure hourly difference marking as ️warning (hPa).
#'
#' @return a plot or ggplotly object.
#'
#' @family plot_tag
#' @examples
#' owd <- setwd(system.file("extdata", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = TRUE)
#' setwd(owd)
#'
#' plot_tag_pressure(tag, plot_plotly = FALSE)
#'
#' owd <- setwd(system.file("extdata", package = "GeoPressureR"))
#' tag <- tag_label(tag, quiet = TRUE)
#' setwd(owd)
#'
#' plot_tag_pressure(tag)
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
      ggplot2::aes(x = .data$date, y = .data$value),
      color = "grey"
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(name = "Pressure (hPa)") +
    ggplot2::theme(legend.position = "none")

  # Only if tag is labelled
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
          cli::cli_bullets(c("!" = "There are only {.val {pressure_length$Freq[id_length[i]]}} \\
            datapoint{?s} for the stationary period \\
                            {.val {pressure_length$stap_id[id_length[i]]}}"))
        }
      } else {
        cli::cli_bullets(c("v" = "All stationary periods have more than \\
                              {.val {warning_stap_length}} datapoints."))
      }
    }

    # Pressure difference
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
        cli::cli_bullets(c(
          ">" = "{.val {nrow(pres_diff)}} timestamp{?s} show{?s/} abnormal hourly change in \\
        pressure (i.e., >{.val {warning_pressure_diff}}hPa):"
        ))
        for (i in seq_len(min(nrow(pres_diff), pressure_diff_max_display))) {
          cli::cli_bullets(
            c("!" = "{pres_diff$date[i]} | stap: {pres_diff$stap_id[i]} | \\
                                  {.val {round(pres_diff$value[i],1)}} hPa ")
          )
        }
        if (nrow(pres_diff) > pressure_diff_max_display) {
          cli::cli_bullets(c(">" = "{.val {nrow(pres_diff)-pressure_diff_max_display}} more \\
                             timestamp{?s} {?is/are} exceeding the threshold."))
        }
      } else {
        cli::cli_bullets(c("v" = "All hourly changes in pressure are below \\
                               {.val {warning_pressure_diff}} hPa."))
      }
    }


    p <- p +
      ggplot2::geom_point(
        data = tag$pressure[tag$pressure$label == "discard", ],
        ggplot2::aes(x = .data$date, y = .data$value),
        colour = "black"
      ) +
      ggplot2::geom_line(
        data = pres,
        ggplot2::aes(x = .data$date, y = .data$value, color = .data$stapelev)
      ) +
      ggplot2::geom_point(
        data = pres_diff,
        ggplot2::aes(x = .data$date, y = .data$value_avg),
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
#' This function display a plot of acceleration time series recorded by a tag
#'
#' @param tag a GeoPressureR `tag` object
#' @param plot_plotly logical to use `plotly`
#' @param label_auto logical to compute and plot the flight label using `tag_label_auto()`. Only if
#' labels are not already present on tag$acceleration$label
#' @inheritParams tag_label_auto
#'
#' @return a plot or ggplotly object.
#'
#' @family plot_tag
#' @examples
#' setwd(system.file("extdata", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = TRUE)
#'
#' plot_tag_acceleration(tag)
#'
#' @export
plot_tag_acceleration <- function(tag,
                                  plot_plotly = TRUE,
                                  label_auto = TRUE,
                                  min_duration = 30) {
  tag_assert(tag)
  assertthat::assert_that(assertthat::has_name(tag, "acceleration"))

  # If not label, use default auto_label
  if (!("label" %in% names(tag$acceleration)) && label_auto) {
    tag <- tag_label_auto(tag, min_duration = min_duration)
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = tag$acceleration,
      ggplot2::aes(x = .data$date, y = .data$value),
      color = "black"
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(name = "Acceleration") +
    ggplot2::theme(legend.position = "none")

  if ("label" %in% names(tag$acceleration)) {
    p <- p +
      ggplot2::geom_point(
        data = tag$acceleration[tag$acceleration$label == "flight", ],
        ggplot2::aes(x = .data$date, y = .data$value),
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
#' This function display a plot of light time series recorded by a tag
#'
#' @param tag a GeoPressureR `tag` object
#' @param plot_plotly logical to use `plotly`
#' @param transform_light logical to display a log transformation of light
#'
#' @return a plot or ggplotly object.
#'
#' @family plot_tag
#' @examples
#' setwd(system.file("extdata", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = TRUE)
#'
#' plot_tag_light(tag)
#'
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
      ggplot2::aes(x = .data$date, y = .data$value),
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
        ggplot2::aes(xintercept = .data$datetime, color = .data$twilight)
      ) +
      ggplot2::scale_color_manual(values = c("sunrise" = "#FFD700", "sunset" = "#FF4500"))
  }

  if (plot_plotly) {
    return(plotly::ggplotly(p, dynamicTicks = TRUE))
  } else {
    return(p)
  }
}


#' Plot temperature data of a `tag`
#'
#' This function display a plot of temperature time series recorded by a tag
#'
#' @param tag a GeoPressureR `tag` object
#' @param plot_plotly logical to use `plotly`
#' @param label_auto logical to compute and plot the flight label using `tag_label_auto()`. Only if
#' labels are not already present on tag$temperature$label
#' @inheritParams tag_label_auto
#'
#' @return a plot or ggplotly object.
#'
#' @family plot_tag
#' @examples
#' setwd(system.file("extdata", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = TRUE)
#'
#' plot_tag_temperature(tag)
#'
#' @export
plot_tag_temperature <- function(tag,
                                 plot_plotly = TRUE,
                                 label_auto = TRUE,
                                 min_duration = 30) {
  tag_assert(tag)
  assertthat::assert_that(assertthat::has_name(tag, "temperature"))

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = tag$temperature,
      ggplot2::aes(x = .data$date, y = .data$value),
      color = "black"
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(name = "Temparature") +
    ggplot2::theme(legend.position = "none")

  if (plot_plotly) {
    return(plotly::ggplotly(p, dynamicTicks = TRUE))
  } else {
    return(p)
  }
}

#' Plot twilight data of a `tag`
#'
#' This function display a plot of twilight time series recorded by a tag
#'
#' @param tag a GeoPressureR `tag` object
#' @param twilight_line a twilight data.frame typically created with `path2twilight()` which is
#' displayed as a line
#' @param plot_plotly logical to use `plotly`
#' @param transform_light logical to display a log transformation of light
#'
#' @return a plot object.
#'
#' @family plot_tag
#' @examples
#' setwd(system.file("extdata", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = TRUE)
#'
#' plot_tag_pressure(tag, plot_plotly = TRUE)
#'
#' tag <- tag_label(tag, quiet = TRUE)
#'
#' plot_tag_pressure(tag)
#'
#' @export
plot_tag_twilight <- function(tag,
                              twilight_line = NULL,
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
  df$time <- as.POSIXct(strptime(mat$time, "%H:%M")) # factor(mat$time, levels = mat$time)
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
      ggplot2::aes(x = .data$date, y = .data$time, fill = .data$light)
    ) +
    ggplot2::scale_fill_gradient(low = "black", high = "white")

  if ("twilight" %in% names(tag)) {
    twl <- tag$twilight
    twl$date <- as.Date(twl$twilight)
    twl$time <- as.POSIXct(strptime(format(twl$twilight, "%H:%M"), "%H:%M"))

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
          ggplot2::aes(x = .data$date, y = .data$time),
          colour = "red",
          size = 2,
          shape = 16
        )
    } else {
      col <- RColorBrewer::brewer.pal(9, "Set1")

      if ("stap_id" %in% names(twl)) {
        twl$stap_id <- factor(round(twl$stap_id))
        p <- p +
          ggplot2::geom_point(
            data = twl,
            ggplot2::aes(x = .data$date, y = .data$time, colour = .data$stap_id),
            size = 2,
            shape = 16
          ) +
          ggplot2::scale_color_manual(
            values = col[seq_along(unique(twl$stap_id)) %% length(col) + 1]
          )
      } else {
        p <- p +
          ggplot2::geom_point(
            data = twl,
            ggplot2::aes(x = .data$date, y = .data$time),
            colour = "lightyellow",
            size = 2,
            shape = 16
          )
      }


    }
    p <- p +
      ggplot2::geom_point(
        data = twl[twl$discard, ],
        ggplot2::aes(x = .data$date, y = .data$time),
        size = 3,
        shape = 4,
        stroke = 2,
        colour = "yellow"
      )
  }

  if (!is.null(twilight_line)) {
    twll <- twilight_line
    twll$date <- as.Date(twll$twilight)
    twll$time <- as.POSIXct(strptime(format(twll$twilight, "%H:%M"), "%H:%M"))
    twll$stap_id <- factor(round(twll$stap_id))

    p <- p +
      ggplot2::geom_line(
        data = twll,
        ggplot2::aes(x = .data$date, y = .data$time, group = .data$rise),
        size = 1,
        color = ifelse(twll$rise, "brown", "lightgreen")
      )
  }

  p <- p +
    ggplot2::theme_bw() +
    ggplot2::scale_y_datetime(
      name = "Time",
      date_breaks = "1 hour",
      date_labels = "%H:%M",
      expand = c(0, 0)
    ) +
    ggplot2::scale_x_date(name = "Date", expand = c(0, 0))

  if (plot_plotly) {
    return(plotly::ggplotly(p, dynamicTicks = TRUE))
  } else {
    # Setting the breaks seems to mess up plotly
    return(p)
  }
}
