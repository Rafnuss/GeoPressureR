
#' Plot `tag`
#'
#' This function display the basic information on a tag list
#
#' @param type type of the plot to display. One of "pressure", "acceleration", "light", "twilight"
#' or "map"
#' @param plot_plotly Logical to display interactive plot with `plotly`
#' @inheritParams tag_create
#' @inheritDotParams plot_tag_pressure
#' @inheritDotParams plot_tag_acceleration
#' @inheritDotParams plot_tag_light
#' @inheritDotParams plot_tag_twilight
#' @inheritDotParams plot_tag_map
#'
#' @return `tag` is returned invisibly and unchanged
#' @family tag
#' @method plot tag
#' @export
plot.tag <- function(tag, type = "pressure", ...) {
  authorized_type <- list(
    "pressure" = plot_tag_pressure,
    "acceleration" = plot_tag_acceleration,
      "light" = plot_tag_light,
      "twilight" = plot_tag_twilight,
      "map" = plot_tag_map
    )

  if (type %in% names(authorized_type)){
    authorized_type[[type]](tag,...)
  } else {
    cli::cli_abort(c(
      "x" = "The type {.val {type}} is not known",
      ">" = "{.var type} should be one of {.val {names(authorized_type)}}"
    ))
  }
}

#' Plot pressure data of a `tag`
#'
#' This function display a plot of pressure timeseries recorded by a tag
#
#' @inheritParams tag_create
#' @inheritParams plot.tag
#' @param stap_length_danger Threshold number of pressure datapoints flagged as ️danger (hourly).
#' @param stap_length_warning Threshold number of pressure datapoints flagged as ️warning (hourly).
#' @param pressure_diff_danger Threshold of pressure hourly difference marking as ️danger (hPa)
#' @param pressure_diff_warning Threshold of pressure hourly difference marking as ️warning (hPa)
#' @export
plot_tag_pressure <- function(tag,
                              plot_plotly = TRUE,
                              display_check = TRUE,
                              pressure_diff_danger = 5,
                              pressure_diff_warning = 3,
                              stap_length_danger = 6,
                              stap_length_warning = 12,
                              ...) {
  tag_assert(tag)
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = tag$pressure,
      ggplot2::aes(x = date, y = value),
      color = "grey"
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(name = "Pressure (hPa)") +
    ggplot2::theme(legend.position = "none")

  # Only if tag is labeled
  if ("stap" %in% names(tag)){
    # Create a "fake" tag
    tag$extent = c(-180, 180, -90, 90)
    tag$scale = 1
    tag$stap$include = TRUE

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

    id_length <- pressure_length$stap_id[pressure_length$Freq <= stap_length_warning]
    if (display_check){
      cli::cli_h3("Stationary periods")
      if (length(id_length) > 0) {
        for (i in seq_len(length(id_length))) {
          if (pressure_length$Freq[id_length[i]] <= stap_length_danger) {
            cli::cli_alert_danger("There are only {.val {pressure_length$Freq[id_length[i]]}} \\
            datapoint{?s} for the stationary period {.val {pressure_length$stap_id[id_length[i]]}}")
          } else {
            cli::cli_alert_warning("There are only {.val {pressure_length$Freq[id_length[i]]}} \\
            datapoint{?s} for the stationary period {.val {pressure_length$stap_id[id_length[i]]}}")
          }
        }
      } else {
        cli::cli_alert_success("All stationary periods have more than {stap_length_warning} \\
                               datapoints.")
      }
    }

    # cli::cli_rule("Pressure difference")
    id_diff_1hr =
      pres_diff <- data.frame(
        value = abs(diff(pres$value)),
        value_avg = utils::head(pres$value, -1) + diff(pres$value)/2,
        date = utils::head(pres$date, -1) + diff(pres$date) / 2,
        stap_id = (utils::tail(pres$stap_id, -1) + utils::head(pres$stap_id, -1))/2
      )
    # Only keep the 1 hours difference
    pres_diff <- pres_diff[as.numeric(diff(pres$date), units="hours")==1,]
    # Remove diff overlapping between stationary periods/flight
    pres_diff <- pres_diff[(pres_diff$stap_id %% 1) == 0 & pres_diff$stap_id != 0,]
    # Only keep difference which are above warning limit
    pres_diff <- pres_diff[pres_diff$value >= pressure_diff_warning,]
    # Add status
    pres_diff$status = factor(ifelse(pres_diff$value >= pressure_diff_danger,"danger","warning"))
    # Sort data.frame for displaying top 10 max
    pres_diff <- pres_diff[order(pres_diff$value, decreasing = TRUE),]

    pressure_diff_max_display <- 10

    if (display_check){
      cli::cli_h3("Pressure difference")
      if (nrow(pres_diff) > 0) {
        cli::cli_alert("{.val {nrow(pres_diff)}} timestamps show excessive hourly change in \\
                            pressure (i.e., {.val >{pressure_diff_warning}}hPa): ")
        for (i in seq_len(min(nrow(pres_diff), pressure_diff_max_display))) {
          if (pres_diff$status[i] == "danger") {
            cli::cli_alert_danger("{pres_diff$date[i]} | stap: {pres_diff$stap_id[i]} | \\
                                  {.val {round(pres_diff$value[i],1)}} hPa ")
          } else {
            cli::cli_alert_warning("{pres_diff$date[i]} | stap: {pres_diff$stap_id[i]} | \\
                                  {.val {round(pres_diff$value[i],1)}} hPa ")
          }
        }
        if ( nrow(pres_diff) > pressure_diff_max_display) {
          cli::cli_alert("{.val {nrow(pres_diff)-pressure_diff_max_display}} more \\
                             timestamps are exceeding the threshold.")
        }
      } else {
        cli::cli_alert_success("All hourly change in pressure are below \\
                               {.val {pressure_diff_warning}} hPa.")
      }
    }

    p <- p +
      ggplot2::geom_point(
        data = tag$pressure[tag$pressure$label == "discard", ],
        ggplot2::aes(x = date, y = value),
        colour = "black"
      ) +
      ggplot2::geom_line(
        data = pres,
        ggplot2::aes(x = date, y = value, color = stapelev)
      ) +
      ggplot2::geom_point(
        data = pres_diff[pres_diff$status == "danger", ],
        ggplot2::aes(x = date, y = value_avg),
        fill = "red", shape = 23, size = 2
      ) +
      ggplot2::geom_point(
        data = pres_diff[pres_diff$status == "warning", ],
        ggplot2::aes(x = date, y = value_avg),
        fill = "orange", shape = 24, size = 2
      )
  }

  if (plot_plotly){
    return(plotly::ggplotly(p, dynamicTicks = TRUE))
  } else {
    return(p)
  }
}

#' Plot acceleration data of a `tag`
#'
#' This function display a plot of acceleration timeseries recorded by a tag
#'
#' @inheritParams tag_create
#' @inheritParams plot.tag
#' @export
plot_tag_acceleration <- function(tag,
                                  plot_plotly = TRUE){
  tag_assert(tag)
  assertthat::assert_that(assertthat::has_name(tag, "acceleration"))

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = tag$acceleration,
      ggplot2::aes(x = date, y = value),
      color = "grey"
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(name = "Acceleration") +
    ggplot2::theme(legend.position = "none")

  # Only if tag is labeled
  if ("stap" %in% names(tag)){
    p <- p +
      ggplot2::geom_line(
        data = tag$acceleration,
        ggplot2::aes(x = date, y = value),
        color = "grey"
      ) +
      ggplot2::geom_line(
        data = tag$acceleration[tag$acceleration$label=="flight",],
        ggplot2::aes(x = date, y = value),
        fill = "red", shape = 23, size = 2
      ) +
      ggplot2::geom_line(
        data = pres,
        ggplot2::aes(x = date, y = value, color = stapelev)
      )
  }

  if (plot_plotly){
    return(plotly::ggplotly(p, dynamicTicks = TRUE))
  } else {
    return(p)
  }
}


#' Plot light data of a `tag`
#'
#' This function display a plot of light timeseries recorded by a tag
#'
#' @inheritParams tag_create
#' @inheritParams plot.tag
#' @export
plot_tag_light <- function(tag,
                           transform_light = TRUE,
                           plot_plotly = TRUE){
  tag_assert(tag)
  assertthat::assert_that(assertthat::has_name(tag, "light"))

  l <- tag$light
  if (transform_light){
    l$value  <- log(l$value+0.0001) + abs(min(log(l$value+0.0001)))
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = l,
      ggplot2::aes(x = date, y = value),
      color = "grey"
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(name = "Light") +
    ggplot2::theme(legend.position = "none")

  # Only if twilight are already computed
  if ("twilight" %in% names(tag)){
    twl <- tag$twilight
    twl$datetime <- twl$twilight
    twl$twilight <- ifelse(twl$rise, "sunset", "sunrise")

    p <- p +
      ggplot2::geom_vline(
        data = twl,
        ggplot2::aes(xintercept = datetime, color = twilight)
      ) +
      ggplot2::scale_color_manual(values = c("sunrise" = "#FFD700", "sunset" = "#FF4500"))
  }

  if (plot_plotly){
    return(plotly::ggplotly(p, dynamicTicks = TRUE))
  } else {
    return(p)
  }
}

#' Plot twilight data of a `tag`
#'
#' This function display a plot of twilight timeseries recorded by a tag
#'
#' @inheritParams tag_create
#' @inheritParams plot.tag
#' @export
plot_tag_twilight <- function(tag, transform_light = TRUE, plot_plotly = FALSE){

  tag_assert(tag, "twilight")

  l <- tag$light
  if (transform_light){
    l$value  <- log(l$value+0.0001) + abs(min(log(l$value+0.0001)))
  }

  # Compute the matrix representation of light
  mat = light2mat(l, twl_offset = tag$param$twl_offset)

  # Convert to long format data.fram to be able to plot with ggplot
  df <- as.data.frame(mat$value)
  names(df) <- mat$day
  df$time <- factor(mat$time,  levels = mat$time)
  df_long <- reshape(df,
                     direction = "long",
                     varying = list(head(names(df), -1)),
                     v.names = "light",
                     idvar = "time",
                     timevar = "date",
                     times = head(names(df), -1))
  df_long$date <- as.Date(df_long$date)

  p <- ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = df_long,
      ggplot2::aes(x = date, y = time, fill = light)
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_gradient(low = "black", high = "white") +
    ggplot2::scale_x_date(name = "Date")

  if ("twilight" %in% names(tag)){
    twl <- tag$twilight
    twl$date <- as.Date(twl$twilight)
    twl$time <- format(twl$twilight, "%H:%M")
    twl$stap_id <- factor(round(twl$stap_id))
    if ("label" %in% names(twl)){
      twl$discard <- twl$label == "discard"
    } else {
      twl$discard <-FALSE
    }

    # plotly doesn't like much changing color...
    if (plot_plotly){
      p <- p +
        ggplot2::geom_point(
          data = twl,
          ggplot2::aes(x = date, y = time),
          colour = "red",
          size = 2,
          shape = 16
        )
    } else {
      p <- p +
        ggplot2::geom_point(
          data = twl,
          ggplot2::aes(x = date, y = time, colour = stap_id),
          size = 2,
          shape = 16
        )
    }
    p <- p +
      ggplot2::geom_point(
        data = twl[twl$discard,],
        ggplot2::aes(x = date, y = time),
        size = 3,
        shape = 4,
        stroke = 2,
        colour = "yellow"
      )
  }

  if (plot_plotly){
    return(plotly::ggplotly(p + ggplot2::scale_y_discrete(name = "Time"), dynamicTicks = TRUE))
  } else {
    # Setting the breaks seems to mess up plotly
    p <- p +
      ggplot2::scale_y_discrete(name = "Time", breaks = format(seq(as.POSIXct("2015-1-1 0:00"), as.POSIXct("2015-1-1 23:00"), by = "hour"), "%H:%M"))
      return(p)
  }
}


#' Plot twilight data of a `tag`
#'
#' This function display a plot of twilight timeseries recorded by a tag
#'
#' @inheritParams tag_create
#' @param plot_leaflet Logical defining if the plot is an interactive `leaflet` map or a static
#' `terra::plot` map.
#' @inheritParams tag2likelihood
#' @inheritParams leaflet::addProviderTiles
#' @inheritParams leaflet::colorNumeric
#' @inheritParams leaflet::addRasterImage
#' @inheritParams terra::plot
#' @inheritDotParams terra::plot
#' @export
plot_tag_map <- function(tag,
                         likelihood = NA,
                         plot_leaflet = TRUE,
                         provider = "Stamen.TerrainBackground",
                         palette = "OrRd",
                         opacity = 0.8,
                         legend = FALSE, ...){
  # Check plot
  tag_assert(tag)

  # Retrieve the requested likelihood map as a terra::rast
  r <- tag2rast(tag, likelihood = likelihood)

  if (plot_leaflet){

    l <- leaflet::leaflet(width = "100%") |>
      leaflet::addProviderTiles(provider = provider)

    grp <- paste0(tag$stap$stap_id, " | ",
                  format(tag$stap$start , format = "%d %b %H:%M"), " - ",
                  format(tag$stap$end , format = "%d %b %H:%M"))

    for (i in seq_len(nrow(tag$stap))) {
      l <- l |> leaflet::addRasterImage(r[[i]],
                                        opacity = opacity,
                                        group = grp[i],
                                        method = "ngb",
                                        colors = leaflet::colorNumeric(palette = palette,
                                                                       domain = NULL,
                                                                       na.color = "#00000000",
                                                                       alpha = TRUE))
    }

    l |>
      leaflet::addLayersControl(
        baseGroups = grp,
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
  } else {
    terra::plot(r, legend = legend, ...)
  }
}
