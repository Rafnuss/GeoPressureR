#' Plot a `tag` object
#'
#' @description
#' This function plots a GeoPressureR `tag` object as a time series or a map.
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
#' - `"actogram"`: `plot_tag_actogram()`
#' - `"map_*"` : `plot.map()` with `tag$map_*` as first argument.
#'
#' Refers to these functions for additional parameters and more flexibility in the plotting.
#'
#' @param x a GeoPressureR `tag` object.
#' @param type type of the plot to display. One of `"pressure"`, `"acceleration"`, `"light"`,
#' `"temperature"`, `"twilight"`, `"actogram"`, `"map"`, `"map_pressure"`, `"map_light"`,
#' `"map_pressure_mse"`, `"map_pressure_mask"`, `"mask_water"`, `"temperature_external"`,
#' `"temperature_internal"`. Map can be combined by providing a vector of type.
#' @param ... additional parameters for `plot_tag_pressure()`, `plot_tag_acceleration()`,
#' `plot_tag_light()`, `plot_tag_twilight()` or `plot.map()`. For twilight and actogram plots,
#' use `flip_axes = TRUE` to place time horizontally and `double_plot = TRUE` for a 48-hour
#' double plot.
#'
#' @return a plot, ggplotly or leaflet object.
#'
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |>
#'     tag_label(quiet = TRUE) |>
#'     twilight_create() |>
#'     twilight_label_read()
#' })
#'
#' # By default, plot will display the time series of pressure
#' plot(tag)
#' # Change the `type` to display other sensor
#' plot(tag, type = "acceleration")
#' plot(tag, type = "light")
#' plot(tag, type = "temperature")
#' plot(tag, type = "twilight")
#' plot(tag, type = "actogram")
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

  is_map_type <- function(x) {
    map_types <- grepl("^map_", x) | x == "map"
    map_names <- unlist(lapply(map_type(), `[[`, "name"), use.names = FALSE)
    map_types <- map_types | x %in% map_names
    map_types
  }

  map_priority <- c(
    "map_pressure",
    "map_light",
    "map_pressure_mse",
    "map_pressure_mask",
    "mask_water"
  )
  sensor_priority <- c(
    "pressure",
    "light",
    "acceleration",
    "temperature_external",
    "temperature_internal",
    "temperature"
  )

  dispatch <- list(
    pressure = plot_tag_pressure,
    acceleration = plot_tag_acceleration,
    light = plot_tag_light,
    temperature = plot_tag_temperature,
    temperature_external = function(tag, ...) {
      plot_tag_temperature(tag, variable = "external", ...)
    },
    temperature_internal = function(tag, ...) {
      plot_tag_temperature(tag, variable = "internal", ...)
    },
    twilight = plot_tag_twilight,
    actogram = plot_tag_actogram
  )

  # Define default
  if (is.null(type)) {
    status <- tag_status(tag)
    if (all(c("map_pressure", "map_light") %in% status)) {
      type <- c("map_pressure", "map_light")
    } else if (any(is_map_type(status))) {
      type <- map_priority[map_priority %in% status][1]
      if (is.na(type)) {
        type <- status[is_map_type(status)][1]
      }
    } else {
      type <- sensor_priority[sensor_priority %in% status][1]
    }
  }

  if (all(is_map_type(type))) {
    # Accept type="map" for default map determined by `tag2map` with likelihood = NA
    if (length(type) == 1 && type == "map") {
      type <- NULL
    } else if (length(type) > 1 && "map" %in% type) {
      type <- setdiff(type, "map")
    }

    # Retrieve the map
    map <- tag2map(tag, likelihood = type)

    # plot the map
    return(plot.map(map, ...))
  }

  fn <- dispatch[[type]]
  if (length(type) == 1 && !is.null(fn)) {
    return(fn(tag, ...))
  }

  cli::cli_abort(c(
    "x" = "The type {.val {type}} is not known",
    ">" = "{.var type} should be one of {.val {c('map', 'map_*', names(dispatch))}}"
  ))
}
#' Plot pressure data of a `tag`
#'
#' This function display a plot of pressure time series recorded by a tag
#
#' @param tag a GeoPressureR `tag` object.
#' @param plot_plotly logical to use `plotly`.
#' @param warning_pressure_diff Threshold of pressure hourly difference marking as warning (hPa).
#'
#' @return a plot or ggplotly object.
#'
#' @family plot_tag
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE)
#' })
#'
#' plot_tag_pressure(tag, plot_plotly = FALSE)
#'
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_label(tag, quiet = TRUE)
#' })
#'
#' plot_tag_pressure(tag)
#' @export
plot_tag_pressure <- function(
  tag,
  plot_plotly = TRUE,
  warning_pressure_diff = 3
) {
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
  if ("label" %in% names(tag$pressure)) {
    # Compute hourly processed pressure and warning differences.
    pres <- geopressure_map_preprocess(tag)
    pres_diff <- pressure_diff_warning_data(tag, warning_pressure_diff)

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
        fill = "orange",
        shape = 24,
        size = 2
      )
  }

  plot_tag_finalize(p, plot_plotly)
}

#' Compute warning pressure differences at 1h intervals
#'
#' @param tag a GeoPressureR `tag` object with labels.
#' @param warning_pressure_diff Threshold of pressure hourly difference marking as warning (hPa).
#' @noRd
pressure_diff_warning_data <- function(tag, warning_pressure_diff = 3) {
  assertthat::assert_that(is.numeric(warning_pressure_diff))
  assertthat::assert_that(length(warning_pressure_diff) == 1)
  assertthat::assert_that(!is.na(warning_pressure_diff))

  # Compute the pressure at the hourly scale.
  pres <- geopressure_map_preprocess(tag)

  # Group by stationary period and elevation level.
  pres$stapelev <- factor(pres$stapelev)

  # Compute consecutive pressure differences.
  pres_diff <- data.frame(
    value = abs(diff(pres$value)),
    value_avg = utils::head(pres$value, -1) + diff(pres$value) / 2,
    date = utils::head(pres$date, -1) + diff(pres$date) / 2,
    date_diff = as.numeric(diff(pres$date), units = "hours"),
    same_stapelev = utils::head(pres$stapelev, -1) == utils::tail(pres$stapelev, -1),
    stap_id = (utils::tail(pres$stap_id, -1) + utils::head(pres$stap_id, -1)) / 2
  )

  # Keep only valid 1-hour differences within a stationary period.
  pres_diff <- pres_diff[pres_diff$date_diff == 1, ]
  pres_diff <- pres_diff[pres_diff$same_stapelev, ]
  pres_diff <- pres_diff[(pres_diff$stap_id %% 1) == 0 & pres_diff$stap_id != 0, ]

  # Keep only differences above warning threshold and sort for display.
  pres_diff <- pres_diff[pres_diff$value >= warning_pressure_diff, ]
  pres_diff <- pres_diff[order(pres_diff$value, decreasing = TRUE), ]
  rownames(pres_diff) <- NULL

  pres_diff
}

#' Plot acceleration data of a `tag`
#'
#' This function display a plot of acceleration time series recorded by a tag
#'
#' @param tag a GeoPressureR `tag` object
#' @param variable type of acceleration variable to plot `"activity"` (or `"value"`) or
#' `"mean_acceleration_z"`
#' @param plot_plotly logical to use `plotly`
#' @param label_auto logical to compute and plot the flight label using `tag_label_auto()`. Only if
#' labels are not already present on tag$acceleration$label
#' @inheritParams tag_label_auto
#'
#' @return a plot or ggplotly object.
#'
#' @family plot_tag
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE)
#' })
#'
#' plot_tag_acceleration(tag)
#'
#' @export
plot_tag_acceleration <- function(
  tag,
  variable = "activity",
  plot_plotly = TRUE,
  label_auto = TRUE,
  min_duration = 30
) {
  tag_assert(tag)
  assertthat::assert_that(assertthat::has_name(tag, "acceleration"))

  if (variable == "pitch") {
    cli::cli_abort(c(
      "x" = "{.arg variable = \"pitch\"} has been deprecated.",
      ">" = "Use {.arg variable = \"mean_acceleration_z\"}."
    ))
  }

  variable <- match.arg(
    variable,
    choices = c("activity", "value", "mean_acceleration_z")
  )

  if (variable == "activity") {
    variable <- "value"
  }
  if (
    variable == "mean_acceleration_z" &&
      !assertthat::has_name(tag$acceleration, "mean_acceleration_z")
  ) {
    cli::cli_abort(c(
      "x" = "The acceleration column {.field mean_acceleration_z} is missing in {.field tag$acceleration}.",
      "i" = "Use {.arg variable = \"value\"} or include {.field mean_acceleration_z} in your acceleration data."
    ))
  }

  # If not label, use default auto_label
  if (!("label" %in% names(tag$acceleration)) && label_auto) {
    tag <- tag_label_auto(tag, min_duration = min_duration)
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = tag$acceleration,
      ggplot2::aes(x = .data$date, y = .data[[variable]]),
      color = "black"
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(
      name = glue::glue("Acceleration - {variable}")
    ) +
    ggplot2::theme(legend.position = "none")

  if ("label" %in% names(tag$acceleration)) {
    p <- p +
      ggplot2::geom_point(
        data = tag$acceleration[tag$acceleration$label == "flight", ],
        ggplot2::aes(x = .data$date, y = .data[[variable]]),
        fill = "red",
        shape = 23,
        size = 2,
      )
  }

  plot_tag_finalize(p, plot_plotly)
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
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE)
#' })
#'
#' plot_tag_light(tag)
#'
#' @export
plot_tag_light <- function(tag, transform_light = TRUE, plot_plotly = TRUE) {
  tag_assert(tag)
  assertthat::assert_that(assertthat::has_name(tag, "light"))

  l <- tag$light
  if (transform_light) {
    l$value <- twilight_create_transform(l$value)
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
    twl$twilight <- ifelse(twl$rise, "sunrise", "sunset")

    p <- p +
      ggplot2::geom_vline(
        data = twl,
        ggplot2::aes(xintercept = .data$datetime, color = .data$twilight)
      ) +
      ggplot2::scale_color_manual(
        values = c("sunrise" = "#FFD700", "sunset" = "#FF4500")
      )
  }

  plot_tag_finalize(p, plot_plotly)
}


#' Plot temperature data of a `tag`
#'
#' This function display a plot of temperature time series recorded by a tag
#'
#' @param tag a GeoPressureR `tag` object
#' @param variable temperature variable to plot `"external"` or `"internal"`
#' @param plot_plotly logical to use `plotly`
#' @return a plot or ggplotly object.
#'
#' @family plot_tag
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE)
#' })
#'
#' plot_tag_temperature(tag)
#'
#' @export
plot_tag_temperature <- function(
  tag,
  variable = "external",
  plot_plotly = TRUE
) {
  tag_assert(tag)
  variable <- match.arg(
    variable,
    choices = c("external", "internal", "temperature_external", "temperature_internal")
  )
  if (variable == "temperature_external") {
    variable <- "external"
  } else if (variable == "temperature_internal") {
    variable <- "internal"
  }
  if (variable == "external") {
    assertthat::assert_that(assertthat::has_name(tag, "temperature_external"))
    temp <- tag$temperature_external
  } else {
    assertthat::assert_that(assertthat::has_name(tag, "temperature_internal"))
    temp <- tag$temperature_internal
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = temp,
      ggplot2::aes(x = .data$date, y = .data$value),
      color = "black"
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(name = variable) +
    ggplot2::theme(legend.position = "none")

  plot_tag_finalize(p, plot_plotly)
}

#' Plot twilight data of a `tag`
#'
#' This function display a plot of twilight time series recorded by a tag
#'
#' @param tag a GeoPressureR `tag` object
#' @param twilight_line a twilight data.frame typically created with `path2twilight()` which is
#' displayed as a line
#' @param plot_plotly logical to use `plotly`
#' @param flip_axes logical to place time on the x-axis and dates on the y-axis. Dates increase
#' downward in the flipped display.
#' @param double_plot logical to display a 48-hour window. The second 24-hour half contains the
#' following day's observations.
#' @inheritParams twilight_create
#'
#' @return a ggplot or ggplotly object.
#'
#' @family plot_tag
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE)
#'
#'   plot_tag_twilight(tag, plot_plotly = TRUE)
#'
#'   tag <- tag_label(tag, quiet = TRUE)
#'
#'   plot_tag_twilight(tag)
#' })
#' @export
plot_tag_twilight <- function(
  tag,
  twilight_line = NULL,
  transform_light = TRUE,
  twl_offset = NULL,
  plot_plotly = TRUE,
  flip_axes = FALSE,
  double_plot = FALSE
) {
  # We need to have light data, if twilight is not yet computed, we can still display the mat image
  tag_assert(tag, "light")

  light <- tag$light
  if (transform_light) {
    light$value <- twilight_create_transform(light$value)
  }

  # Use by order of priority: (1) twl_offset provided in this function, (2)
  # tag$param$twilight_create$twl_offset, (3) guess from light data
  twl_offset <- resolve_twl_offset(tag, light, twl_offset)

  if ("twl_time_tolerance" %in% names(tag$param$twilight_create)) {
    twl_time_tolerance <- tag$param$twilight_create$twl_time_tolerance
  } else {
    twl_time_tolerance <- formals(twilight_create)$twl_time_tolerance
  }

  # Compute the matrix representation of light
  mat <- ts2mat(
    light,
    twl_offset = twl_offset,
    twl_time_tolerance = twl_time_tolerance
  )

  # Convert to long format data.frame to be able to plot with ggplot
  mat_long <- ts2mat_to_long(mat, value_name = "light", double_plot = double_plot)
  df_long <- mat_long$data
  mat_time_hour <- mat_long$mat_time_hour
  light_rng <- range(df_long$light, na.rm = TRUE)
  df_long$light[is.na(df_long$light)] <- -1
  x_axis <- if (flip_axes) "time" else "date"
  y_axis <- if (flip_axes) "date" else "time"

  p <- ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = df_long,
      ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]], fill = .data$light)
    ) +
    ggplot2::scale_fill_gradientn(
      colours = c("#CC6677", "black", "white"),
      values = c(0, (light_rng[1] + 1) / (light_rng[2] + 1), 1),
      limits = c(-1, light_rng[2])
    )

  if ("twilight" %in% names(tag)) {
    twl <- tag$twilight
    twl$date <- as.Date(twl$twilight)
    time_hour <- as.numeric(substr(format(twl$twilight, "%H:%M"), 1, 2)) +
      as.numeric(substr(format(twl$twilight, "%H:%M"), 4, 5)) / 60
    time_hour <- time_hour + 24 * (time_hour < mat_time_hour[1])
    twl$time <- as.POSIXct(Sys.Date()) + time_hour * 3600
    if (double_plot) {
      twl <- rbind(twl, transform(twl, date = date - 1, time = time + 24 * 60 * 60))
    }

    if ("label" %in% names(twl)) {
      twl$discard <- twl$label == "discard"
    } else {
      twl$discard <- FALSE
    }

    # plotly doesn't like much changing colour...
    if (plot_plotly) {
      p <- p +
        ggplot2::geom_point(
          data = twl,
          ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]]),
          colour = "yellow",
          size = 4,
          shape = 16
        )
    } else {
      col <- RColorBrewer::brewer.pal(9, "Set1")

      if ("stap_id" %in% names(twl)) {
        twl$stap_id <- factor(round(twl$stap_id))
        p <- p +
          ggplot2::geom_point(
            data = twl,
            ggplot2::aes(
              x = .data[[x_axis]],
              y = .data[[y_axis]],
              colour = .data$stap_id
            ),
            size = 6,
            shape = 16
          ) +
          ggplot2::scale_color_manual(
            values = col[seq_along(unique(twl$stap_id)) %% length(col) + 1]
          )
      } else {
        p <- p +
          ggplot2::geom_point(
            data = twl,
            ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]], color = .data$rise),
            size = 4,
            shape = 16
          ) +
          ggplot2::scale_color_manual(
            values = c("TRUE" = "yellow", "FALSE" = "orange")
          )
      }
    }
    p <- p +
      ggplot2::geom_point(
        data = twl[twl$discard, ],
        ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]]),
        size = 3,
        shape = 4,
        stroke = 2,
        colour = "red"
      )
  }

  if (!is.null(twilight_line)) {
    twll <- twilight_line
    twll$date <- as.Date(twll$twilight)
    time_hour <- as.numeric(substr(format(twll$twilight, "%H:%M"), 1, 2)) +
      as.numeric(substr(format(twll$twilight, "%H:%M"), 4, 5)) / 60
    time_hour <- time_hour + 24 * (time_hour < mat_time_hour[1])
    twll$time <- as.POSIXct(Sys.Date()) + time_hour * 3600
    twll$copy <- 1L
    if (double_plot) {
      twll <- rbind(
        twll,
        transform(
          twll,
          date = date - 1,
          time = time + 24 * 60 * 60,
          copy = 2L
        )
      )
    }
    # Group by rounded stap_id so decimal flight values are attached to the nearest stationary
    # period without linking unrelated segments when stap_id is discontinuous.
    twll$stap_id <- interaction(round(twll$stap_id), twll$copy)

    p <- p +
      ggplot2::geom_line(
        data = twll[twll$rise, ],
        ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]], group = .data$stap_id),
        linewidth = 1,
        color = "brown"
      ) +
      ggplot2::geom_line(
        data = twll[!twll$rise, ],
        ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]], group = .data$stap_id),
        linewidth = 1,
        color = "lightgreen"
      )
  }

  p <- p + ggplot2::theme_bw()
  if (flip_axes) {
    p <- p +
      ggplot2::scale_x_datetime(
        name = "Time",
        date_breaks = "1 hour",
        date_labels = "%H:%M",
        expand = c(0, 0)
      ) +
      ggplot2::scale_y_date(
        name = "Date",
        expand = c(0, 0)
      )
  } else {
    p <- p +
      ggplot2::scale_y_datetime(
        name = "Time",
        date_breaks = "1 hour",
        date_labels = "%H:%M",
        expand = c(0, 0)
      ) +
      ggplot2::scale_x_date(name = "Date", expand = c(0, 0))
  }

  if (flip_axes && !plot_plotly) {
    p <- p + ggplot2::coord_transform(y = "reverse")
  }

  # Setting the breaks seems to mess up plotly
  p <- plot_tag_finalize(p, plot_plotly, autorange = FALSE)
  if (flip_axes && plot_plotly) {
    p <- plot_tag_flip_axes_plotly(p)
  }
  p
}


#' Plot Actogram data of a `tag`
#'
#' This function display a plot of the acceleration time series recorded by a tag
#'
#' @param tag a GeoPressureR `tag` object
#' @param plot_plotly logical to use `plotly`
#' @param flip_axes logical to place time on the x-axis and dates on the y-axis. Dates increase
#' downward in the flipped display.
#' @param double_plot logical to display a 48-hour window. The second 24-hour half contains the
#' following day's observations.
#' @inheritParams twilight_create
#'
#' @return a ggplot or ggplotly object.
#'
#' @family plot_tag
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE)
#'
#'   plot_tag_actogram(tag, plot_plotly = TRUE)
#' })
#' @export
plot_tag_actogram <- function(
  tag,
  twl_offset = NULL,
  plot_plotly = TRUE,
  flip_axes = FALSE,
  double_plot = FALSE
) {
  # We need to have acceleration data
  tag_assert(tag, "acceleration")

  acc <- tag$acceleration

  # Use by order of priority: (1) twl_offset provided in this function, (2)
  # tag$param$twilight_create$twl_offset, (3) guess from light data
  twl_offset <- resolve_twl_offset(tag, acc, twl_offset)

  # Compute the matrix representation of light
  mat <- ts2mat(acc, twl_offset = twl_offset)

  if ("label" %in% names(acc)) {
    matl <- ts2mat(acc, twl_offset = twl_offset, value = "label")
    mat$value[matl$value == "flight"] <- max(acc$value) + 1
  }

  # Convert to long format data.frame to be able to plot with ggplot
  mat_long <- ts2mat_to_long(mat, value_name = "acceleration", double_plot = double_plot)
  df_long <- mat_long$data
  df_long$acceleration[is.na(df_long$acceleration)] <- -1
  x_axis <- if (flip_axes) "time" else "date"
  y_axis <- if (flip_axes) "date" else "time"

  # Make color scale
  pos_acc <- acc$value[acc$value > 0 & is.finite(acc$value)]
  use_kmeans <- length(pos_acc) >= 2 && length(unique(pos_acc)) >= 2
  if (use_kmeans) {
    km <- stats::kmeans(pos_acc, centers = 2)
    acc_low_act <- pos_acc[pos_acc < mean(km$centers)]
    if (length(acc_low_act) == 0) {
      acc_low_act <- pos_acc
    }
    x <- c(
      0,
      min(acc_low_act),
      mean(acc_low_act),
      max(acc_low_act),
      mean(km$centers),
      max(acc$value, na.rm = TRUE)
    )
    rng <- range(x, na.rm = TRUE)
  } else {
    rng <- range(acc$value, na.rm = TRUE)
    if (!all(is.finite(rng))) {
      rng <- c(0, 1)
    }
    if (diff(rng) == 0) {
      rng <- c(rng[1], rng[1] + 1)
    }
    x <- seq(rng[1], rng[2], length.out = 6)
  }
  val <- (x - rng[1]) / diff(rng)

  p <- ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = df_long,
      ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]], fill = .data$acceleration)
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_gradientn(
      colours = c(
        "grey", # Missing data
        "#FFFFFF", # 0 - No activity
        "#B2FFB2", # Low activity (light green)
        "#66FF66", # Medium activity (green)
        "#33CC33", # Higher activity (darker green),
        "#660066", # High activity (purple)
        "#000000" # Continuous activity (black)
      ),
      values = c(0, (x + 1) / (rng[2] + 1)),
      limits = c(-1, rng[2])
    )

  if (flip_axes) {
    p <- p +
      ggplot2::scale_x_datetime(
        name = "Time",
        date_breaks = "1 hour",
        date_labels = "%H:%M",
        expand = c(0, 0)
      ) +
      ggplot2::scale_y_date(
        name = "Date",
        expand = c(0, 0)
      )
  } else {
    p <- p +
      ggplot2::scale_y_datetime(
        name = "Time",
        date_breaks = "1 hour",
        date_labels = "%H:%M",
        expand = c(0, 0)
      ) +
      ggplot2::scale_x_date(name = "Date", expand = c(0, 0))
  }

  if (flip_axes && !plot_plotly) {
    p <- p + ggplot2::coord_transform(y = "reverse")
  }

  p <- plot_tag_finalize(p, plot_plotly)
  if (flip_axes && plot_plotly) {
    p <- plot_tag_flip_axes_plotly(p)
  }
  p
}


#' @noRd
resolve_twl_offset <- function(tag, data, twl_offset) {
  if (!is.null(twl_offset)) {
    return(twl_offset)
  }
  if (
    !is.null(tag$param) &&
      "twilight_create" %in% names(tag$param) &&
      "twl_offset" %in% names(tag$param$twilight_create)
  ) {
    return(tag$param$twilight_create$twl_offset)
  }
  twilight_create_guess_offset(data)
}

#' @noRd
ts2mat_to_long <- function(mat, value_name, double_plot = FALSE) {
  df <- as.data.frame(mat$value)
  names(df) <- mat$day
  mat_time_hour <- as.numeric(substr(mat$time, 1, 2)) +
    as.numeric(substr(mat$time, 4, 5)) / 60
  time_hour <- mat_time_hour + 24 * (mat_time_hour < mat_time_hour[1])
  df$time <- as.POSIXct(Sys.Date()) + time_hour * 3600

  df_long <- stats::reshape(
    df,
    direction = "long",
    varying = list(utils::head(names(df), -1)),
    v.names = value_name,
    idvar = "time",
    timevar = "date",
    times = utils::head(names(df), -1)
  )
  df_long$date <- as.Date(df_long$date)

  if (double_plot) {
    df_long <- rbind(
      df_long,
      transform(
        df_long,
        date = date - 1,
        time = time + 24 * 60 * 60
      )
    )
    mat_time_hour <- c(mat_time_hour, mat_time_hour + 24)
  }

  list(data = df_long, mat_time_hour = mat_time_hour)
}

#' @noRd
plot_tag_flip_axes_plotly <- function(p) {
  p$x$data <- lapply(p$x$data, function(trace) {
    if (inherits(trace$x, "POSIXt")) {
      trace$x <- as.numeric(trace$x) / 3600
    }
    if (!is.null(trace$text)) {
      trace$text <- gsub(
        "[0-9]{4}-[0-9]{2}-[0-9]{2} ([0-9]{2}:[0-9]{2}):[0-9]{2}",
        "\\1",
        trace$text
      )
    }
    trace
  })

  xaxis <- p$x$layout$xaxis
  tickvals <- xaxis$tickvals
  ticktime <- as.POSIXct(tickvals, origin = "1970-01-01", tz = "UTC")
  tick_id <- format(ticktime, "%H:%M") %in% c("00:00", "06:00", "12:00", "18:00")
  xaxis$type <- "linear"
  xaxis$tickmode <- "array"
  xaxis$tickvals <- tickvals[tick_id] / 3600
  xaxis$ticktext <- format(ticktime[tick_id], "%H:%M")
  xaxis$range <- as.numeric(as.POSIXct(xaxis$range, tz = "UTC")) / 3600
  xaxis$categoryorder <- NULL
  xaxis$categoryarray <- NULL
  xaxis$hoverformat <- NULL
  p$x$layout$xaxis <- xaxis
  p$x$layout$yaxis$autorange <- "reversed"
  p
}

#' @noRd
plot_tag_finalize <- function(
  p,
  plot_plotly,
  autorange = TRUE
) {
  if (plot_plotly) {
    p <- plotly::ggplotly(p, dynamicTicks = TRUE)
    if (isFALSE(autorange)) {
      p <- p |>
        plotly::layout(
          xaxis = list(autorange = FALSE),
          yaxis = list(autorange = FALSE)
        )
    }
    return(p)
  }
  p
}
