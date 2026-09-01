server <- function(input, output, session) {
  # Update browser tab title with tag ID
  session$sendCustomMessage(
    "updateTitle",
    glue::glue("GeoPressureViz - {tag$param$id}")
  )

  if (isTRUE(shiny::getShinyOption("stop_on_session_end", TRUE))) {
    session$onSessionEnded(function() {
      stopApp()
    })
  }

  # Extract shorter name for variable
  stap <- tag$stap
  pressure <- tag$pressure

  if (is.null(file_wind)) {
    edge <- NULL
  } else {
    edge <- path2edge(path, tag)

    wind <- edge_add_wind(
      tag,
      edge_s = edge$s,
      edge_t = edge$t,
      file = file_wind
    )
    uv <- stats::aggregate(wind$val * wind$w, wind[c("edge_id", "var")], sum)
    uv <- stats::reshape(uv, idvar = "edge_id", timevar = "var", direction = "wide")
    edge$ws <- (uv$x.u + 1i * uv$x.v) * 3.6
  }

  # Compute resolution of projection
  r <- maps[[1]]
  g <- map_expand(r$extent, r$scale)
  lonInEPSG3857 <- (g$lon * 20037508.34 / 180)
  latInEPSG3857 <- (log(tan((90 + g$lat) * pi / 360)) / (pi / 180)) *
    (20037508.34 / 180)
  fac_res_proj <- 4
  res_proj <- c(
    stats::median(diff(lonInEPSG3857)),
    min(abs(diff(latInEPSG3857))) / fac_res_proj
  )
  origin_proj <- c(stats::median(lonInEPSG3857), stats::median(latInEPSG3857))

  # Convert lat-lon into ind
  latlon2ind <- function(lat, lon) {
    lat_ind <- round(
      stats::approx(g$lat, seq(1, length(g$lat)), lat, rule = 2)$y
    )
    lon_ind <- round(
      stats::approx(g$lon, seq(1, length(g$lon)), lon, rule = 2)$y
    )
    ind <- (lon_ind - 1) * g$dim[1] + lat_ind
    return(ind)
  }

  ## Reactive variable ----

  reactVal <- reactiveValues(
    path = path,
    edge = edge,
    pressurepath = pressurepath,
    pressure_xmin = NULL,
    pressure_xmax = NULL,
    isEdit = FALSE # if editing position
  )

  shiny::observe({
    # Store current path as path_geopressureviz in shiny options
    shiny::shinyOptions(path_geopressureviz = reactVal$path)
  })

  # return the map
  map_display <- shiny::reactive({
    if (is.null(input$map_source)) {
      return(NA)
    }
    r <- rast.map(maps[[input$map_source]])
    # Robust normalization across all layers. `terra::minmax()` returns per-layer ranges,
    # and `diff(minmax)` is not "max-min" for multi-layer rasters.
    r_min <- suppressWarnings(terra::global(r, "min", na.rm = TRUE)[1, 1])
    r_max <- suppressWarnings(terra::global(r, "max", na.rm = TRUE)[1, 1])
    if (!is.finite(r_min) || !is.finite(r_max) || r_max <= r_min) {
      r_norm <- r
    } else {
      r_norm <- (r - r_min) / (r_max - r_min)
    }
    terra::project(
      r_norm,
      "epsg:3857",
      method = "near",
      res = res_proj,
      origin = origin_proj
    )
  }) |>
    bindEvent(input$map_source)

  # list of the stap_id which are above the threashold of duration and included in the model
  stap_id_include <- shiny::reactive({
    min_dur_stap <- ifelse(
      is.na(input$min_dur_stap),
      0,
      as.numeric(input$min_dur_stap)
    )
    which(stap$duration >= min_dur_stap & stap$include)
  }) |>
    bindEvent(input$min_dur_stap)

  # Precompute flight durations for current included stap ids
  flight_dur_reactive <- shiny::reactive({
    stap2flight(stap, stap_id_include())$duration
  }) |>
    bindEvent(stap_id_include())

  # index of the current stap_id in the stap_id_include (so not index in all stap_id, only the one to use)
  idx <- shiny::reactive({
    which(stap_id_include() == input$stap_id)
  }) |>
    bindEvent(input$stap_id)

  ## Render ----
  output$map <- leaflet::renderLeaflet({
    map <- leaflet::leaflet() |>
      leaflet::addMapPane("raster_pane", zIndex = 210) |>
      leaflet::addProviderTiles(
        "CartoDB.DarkMatterNoLabels",
        group = "Dark Matter"
      ) |>
      # options = providerTileOptions(noWrap = TRUE)
      leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
      leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Topography") |>
      leaflet::addLayersControl(
        baseGroups = c("Dark Matter", "Satellite", "Topography"),
        position = c("topleft")
      )
  })
  output$tag_id <- shiny::renderUI({
    return(shiny::HTML(glue::glue("<h3 style='margin:0;'>", tag$param$id, "</h3>")))
  })

  # Small helper to compute distance (km) and flight duration (hours) between two stap indices
  flight_info <- function(idx, nextprev) {
    nextnext_id <- stap_id_include()[idx + nextprev]
    curr_id <- as.numeric(input$stap_id)
    nb_fl <- abs(nextnext_id - curr_id)

    dist_km <- geosphere::distGeo(
      reactVal$path[nextnext_id, c("lon", "lat")],
      reactVal$path[curr_id, c("lon", "lat")]
    ) /
      1000

    fl_dur <- flight_dur_reactive()[min(idx, idx + nextprev)]

    flights_val <- nb_fl
    flights_unit <- if (nb_fl > 1) "flights" else "flight"
    duration_val <- round(fl_dur, 1)
    duration_unit <- "hrs"
    distance_val <- round(dist_km)
    distance_unit <- "km"
    speed_val <- if (!is.na(fl_dur) && fl_dur > 0) {
      round(dist_km / fl_dur)
    } else {
      "—"
    }
    speed_unit <- if (identical(speed_val, "—")) "" else "km/h"

    label <- if (nextprev > 0) "Next flight" else "Previous flight"
    dot_col <- stap$col[nextnext_id]
    if (is.null(dot_col) || is.na(dot_col) || dot_col == "") {
      dot_col <- "#999999"
    }

    shiny::div(
      class = "gpv-flight-card",
      style = glue::glue("border-color:{dot_col};"),
      shiny::div(
        class = "gpv-flight-title",
        shiny::span(class = "gpv-color-dot", style = glue::glue("background-color:{dot_col};")),
        shiny::tags$span(label)
      ),
      shiny::div(
        class = "gpv-flight-metrics",
        shiny::div(
          class = "gpv-flight-metric",
          shiny::tags$i(class = "bi bi-airplane"),
          shiny::tags$span(
            class = "gpv-flight-metric-main",
            shiny::tags$span(class = "gpv-flight-metric-value", flights_val),
            shiny::tags$span(class = "gpv-flight-metric-unit", flights_unit)
          )
        ),
        shiny::div(
          class = "gpv-flight-metric",
          shiny::tags$i(class = "bi bi-hourglass-split"),
          shiny::tags$span(
            class = "gpv-flight-metric-main",
            shiny::tags$span(class = "gpv-flight-metric-value", duration_val),
            shiny::tags$span(class = "gpv-flight-metric-unit", duration_unit)
          )
        ),
        shiny::div(
          class = "gpv-flight-metric",
          shiny::tags$i(class = "bi bi-signpost-2"),
          shiny::tags$span(
            class = "gpv-flight-metric-main",
            shiny::tags$span(class = "gpv-flight-metric-value", distance_val),
            shiny::tags$span(class = "gpv-flight-metric-unit", distance_unit)
          )
        ),
        shiny::div(
          class = "gpv-flight-metric",
          shiny::tags$i(class = "bi bi-speedometer2"),
          shiny::tags$span(
            class = "gpv-flight-metric-main",
            shiny::tags$span(class = "gpv-flight-metric-value", speed_val),
            shiny::tags$span(class = "gpv-flight-metric-unit", speed_unit)
          )
        )
      )
    )
  }

  output$flight_prev_info <- shiny::renderUI({
    shiny::req(input$stap_id)
    if (idx() == 1) {
      return(shiny::HTML(""))
    }
    flight_info(idx(), -1)
  })

  output$flight_next_info <- shiny::renderUI({
    shiny::req(input$stap_id)
    if (idx() == length(stap_id_include())) {
      return(shiny::HTML(""))
    }
    flight_info(idx(), +1)
  })

  output$pressure_plot <- plotly::renderPlotly({
    # Plotting too many discarded points makes Plotly very slow.
    # We only cap discards for large displayed time windows; when zoomed in, keep them all.
    max_discard_points <- 5000L
    discard_cap_min_days <- 60

    disc <- subset(pressure, label == "discard")

    xmin <- isolate(reactVal$pressure_xmin)
    xmax <- isolate(reactVal$pressure_xmax)
    if (!is.null(xmin) && !is.null(xmax) && !anyNA(c(xmin, xmax))) {
      disc <- disc[disc$date >= xmin & disc$date <= xmax, , drop = FALSE]
    }

    view_days <- NA_real_
    if (!is.null(xmin) && !is.null(xmax) && !anyNA(c(xmin, xmax))) {
      view_days <- as.numeric(difftime(xmax, xmin, units = "days"))
    }

    if (
      is.finite(view_days) && view_days >= discard_cap_min_days && nrow(disc) > max_discard_points
    ) {
      keep <- unique(pmax.int(
        1L,
        pmin.int(nrow(disc), as.integer(round(seq(1, nrow(disc), length.out = max_discard_points))))
      ))
      disc <- disc[keep, , drop = FALSE]
    }

    p <- ggplot2::ggplot() +
      ggplot2::geom_line(
        data = pressure,
        ggplot2::aes(x = date, y = value),
        colour = "grey"
      ) +
      ggplot2::geom_point(
        data = disc,
        ggplot2::aes(x = date, y = value),
        colour = "black"
      ) +
      ggplot2::theme_bw()

    if (nrow(reactVal$pressurepath) > 0) {
      pp <- reactVal$pressurepath
      group_var <- if ("stap_id" %in% names(pp)) {
        "stap_id"
      } else if ("stap_ref" %in% names(pp)) {
        "stap_ref"
      } else {
        NULL
      }

      y_var <- if ("surface_pressure_norm" %in% names(pp)) {
        "surface_pressure_norm"
      } else if ("surface_pressure" %in% names(pp)) {
        "surface_pressure"
      } else {
        NULL
      }

      has_linetype <- "linetype" %in% names(pp)

      p <- p +
        ggplot2::geom_line(
          data = pp,
          mapping = if (!is.null(group_var) && !is.null(y_var) && isTRUE(has_linetype)) {
            ggplot2::aes(
              x = .data$date,
              y = .data[[y_var]],
              color = .data$col,
              group = .data[[group_var]],
              linetype = .data$linetype
            )
          } else if (!is.null(group_var) && !is.null(y_var)) {
            ggplot2::aes(
              x = .data$date,
              y = .data[[y_var]],
              color = .data$col,
              group = .data[[group_var]]
            )
          } else if (!is.null(y_var)) {
            ggplot2::aes(
              x = .data$date,
              y = .data[[y_var]],
              color = .data$col
            )
          } else {
            ggplot2::aes(
              x = .data$date,
              y = .data$value,
              color = .data$col
            )
          }
        ) +
        ggplot2::scale_color_identity()
    }

    tooltip_vars <- if (
      isTRUE(nrow(reactVal$pressurepath) > 0) && "linetype" %in% names(reactVal$pressurepath)
    ) {
      c("x", "y", "linetype")
    } else {
      c("x", "y")
    }

    plotly::ggplotly(
      p,
      dynamicTicks = TRUE,
      height = 300,
      tooltip = tooltip_vars,
      source = "pressure_plot"
    ) |>
      plotly::layout(
        showlegend = FALSE,
        uirevision = "pressure-plot",
        yaxis = list(title = "Pressure [hPa]")
      ) |>
      plotly::config(
        scrollZoom = FALSE, # Disable built-in scroll zoom
        displayModeBar = TRUE,
        # doubleClick = "reset", # Reset zoom on double click
        modeBarButtonsToRemove = list(
          "zoomIn2d",
          "zoomOut2d",
          # "autoScale2d",
          # "resetScale2d",
          "toImage",
          "hoverClosestCartesian",
          "hoverCompareCartesian",
          "select2d",
          "lasso2d"
        ),
        displaylogo = FALSE
      )
  })

  # Track current pressure plot x-range so we can cap discards only for large windows.
  shiny::observeEvent(plotly::event_data("plotly_relayout", source = "pressure_plot"), {
    ev <- plotly::event_data("plotly_relayout", source = "pressure_plot")
    if (is.null(ev)) {
      return()
    }

    xmin <- NULL
    xmax <- NULL
    if (!is.null(ev[["xaxis.range[0]"]]) && !is.null(ev[["xaxis.range[1]"]])) {
      xmin <- ev[["xaxis.range[0]"]]
      xmax <- ev[["xaxis.range[1]"]]
    } else if (!is.null(ev[["xaxis.range"]]) && length(ev[["xaxis.range"]]) >= 2) {
      xmin <- ev[["xaxis.range"]][[1]]
      xmax <- ev[["xaxis.range"]][[2]]
    }

    if (is.null(xmin) || is.null(xmax)) {
      return()
    }
    xmin <- as.POSIXct(xmin, tz = "UTC")
    xmax <- as.POSIXct(xmax, tz = "UTC")
    if (anyNA(c(xmin, xmax))) {
      return()
    }

    # De-dup updates to avoid re-render loops.
    if (
      !is.null(reactVal$pressure_xmin) &&
        !is.null(reactVal$pressure_xmax) &&
        !anyNA(c(reactVal$pressure_xmin, reactVal$pressure_xmax)) &&
        isTRUE(all.equal(as.numeric(reactVal$pressure_xmin), as.numeric(xmin))) &&
        isTRUE(all.equal(as.numeric(reactVal$pressure_xmax), as.numeric(xmax)))
    ) {
      return()
    }

    reactVal$pressure_xmin <- xmin
    reactVal$pressure_xmax <- xmax
  })

  ## ObserveEvents ----
  # Same order than the ui

  shiny::observeEvent(input$full_track, {
    if (input$full_track) {
      shinyjs::hide(id = "stap_info_view", anim = TRUE)
      shinyjs::show(id = "track_info_view", anim = TRUE)
    } else {
      shinyjs::show(id = "stap_info_view", anim = TRUE)
      shinyjs::hide(id = "track_info_view", anim = TRUE)
      if (input$min_dur_stap > 0) {
        shinyjs::show(id = "edit_position_interpolate")
      } else {
        shinyjs::hide(id = "edit_position_interpolate")
      }
    }
  })

  shiny::observeEvent(input$min_dur_stap, {
    if (length(stap_id_include()) > 0) {
      choices <- as.character(stap_id_include())
      names(choices) <- glue::glue(
        "#{stap_id_include()} ({round(stap$duration[stap_id_include()], 1)} d.)"
      )
    } else {
      choices <- character()
    }
    session$onFlushed(function() {
      shiny::updateSelectizeInput(session, "stap_id", choices = choices)
    })
  })

  shiny::observeEvent(input$previous_position, {
    idx_new <- min(max(idx() - 1, 1), length(stap_id_include()))
    shiny::updateSelectizeInput(
      session,
      "stap_id",
      selected = as.character(stap_id_include()[idx_new])
    )
  })

  shiny::observeEvent(input$next_position, {
    idx_new <- min(max(idx() + 1, 1), length(stap_id_include()))
    shiny::updateSelectizeInput(
      session,
      "stap_id",
      selected = as.character(stap_id_include()[idx_new])
    )
  })

  shiny::observeEvent(input$edit_position, {
    if (reactVal$isEdit) {
      reactVal$isEdit <- FALSE
      shiny::updateActionButton(session, "edit_position", label = "Start editing")
      removeClass("edit_position", "primary")
    } else {
      reactVal$isEdit <- TRUE
      shiny::updateActionButton(session, "edit_position", label = "Stop editing")
      addClass("edit_position", "primary")
    }
  })

  shiny::observeEvent(input$map_click, {
    click <- input$map_click
    if (is.null(click)) {
      return()
    }
    if (!reactVal$isEdit) {
      return()
    }
    if (!input$full_track) {
      reactVal$path[as.numeric(input$stap_id), c("lon", "lat")] <- c(
        click$lng,
        click$lat
      )
      reactVal$path[as.numeric(input$stap_id), "ind"] <- latlon2ind(
        click$lat,
        click$lng
      )

      if (input$edit_position_interpolate) {
        if (idx() != 1) {
          stap_id_prev <- stap_id_include()[idx() - 1]
        } else {
          stap_id_prev <- as.numeric(input$stap_id)
        }
        if (idx() != length(stap_id_include())) {
          stap_id_next <- stap_id_include()[idx() + 1]
        } else {
          stap_id_next <- as.numeric(input$stap_id)
        }

        stap_prev_to_next <- seq(stap_id_prev, stap_id_next)

        # Cummulate the flight duration to get a proxy of the over distance covered
        total_flight <- cumsum(as.numeric(c(
          0,
          flight$duration[stap_prev_to_next]
        )))

        stap_interp <- !(stap_prev_to_next %in%
          c(stap_id_prev, as.numeric(input$stap_id), stap_id_next))

        path_prev_to_next <- reactVal$path[stap_prev_to_next, ]

        # Interpolate the lat and lon indices separately using `total_flight` as a spacing between
        # position
        path_prev_to_next$lon[stap_interp] <- round(
          stats::approx(
            total_flight[!stap_interp],
            path_prev_to_next$lon[!stap_interp],
            total_flight[stap_interp]
          )$y
        )
        path_prev_to_next$lat[stap_interp] <- round(
          stats::approx(
            total_flight[!stap_interp],
            path_prev_to_next$lat[!stap_interp],
            total_flight[stap_interp]
          )$y
        )

        reactVal$path[stap_prev_to_next, ] <- path_prev_to_next
      }
    }
  })

  # Map
  shiny::observe({
    proxy <- leaflet::leafletProxy("map") |>
      leaflet::clearShapes() |>
      leaflet::clearImages() |>
      leaflet::clearMarkers()
    stap_model <- stap[stap_id_include(), ]
    path_model <- reactVal$path[stap_id_include(), c("lon", "lat")]
    fl_dur <- flight_dur_reactive()
    if (is.null(fl_dur)) {
      return()
    }
    if (input$full_track) {
      proxy <- proxy |>
        leaflet::addPolylines(
          lng = path_model$lon,
          lat = path_model$lat,
          opacity = 1,
          color = "#FFF",
          weight = 3
        ) |>
        leaflet::addCircleMarkers(
          lng = path_model$lon,
          lat = path_model$lat,
          fillOpacity = 1,
          radius = stap_model$duration^(0.3) * 10,
          weight = 1,
          color = "#FFF",
          label = glue::glue(
            "#{stap_model$stap_id}, {round(stap_model$duration, 1)} days"
          ),
          fillColor = stap_model$col
        ) |>
        leaflet::fitBounds(
          min(path_model$lon),
          min(path_model$lat),
          max(path_model$lon),
          max(path_model$lat),
          options = list(paddingBottomRight = c(300, 300))
        )
    } else {
      # Track from
      map_stap_id <- map_display()[[as.numeric(input$stap_id)]]
      if (!is.null(map_stap_id)) {
        key <- map_type_key[[input$map_source]]
        spec <- GeoPressureR::map_type()[[key]]
        if (is.null(spec)) {
          spec <- GeoPressureR::map_type()[["unknown"]]
        }
        spec_dark <- spec[["dark"]]

        proxy <- proxy |>
          leaflet::addRasterImage(
            map_stap_id,
            opacity = 0.8,
            colors = leaflet::colorNumeric(
              palette = spec_dark$palette,
              domain = NULL,
              reverse = isTRUE(spec_dark$reverse),
              na.color = "#00000000",
              alpha = TRUE
            ),
            project = FALSE,
            options = leaflet::pathOptions(pane = "raster_pane")
          )
      }
      proxy <- proxy |>
        leaflet::addPolylines(
          lng = path_model$lon,
          lat = path_model$lat,
          opacity = 0.1,
          color = "#FFF",
          weight = 3
        ) |>
        leaflet::addCircles(
          lng = path_model$lon,
          lat = path_model$lat,
          fillOpacity = 0.1,
          fillColor = "#FFF",
          weight = 0,
          radius = stap_model$duration^(0.3) * 10
        )

      if (idx() != 1) {
        path_lon_ws <- NULL
        path_lat_ws <- NULL
        if (!is.null(reactVal$edge)) {
          stap_id_prev <- stap_id_include()[idx() - 1]
          tmp <- reactVal$edge[
            seq(stap_id_prev, as.numeric(input$stap_id) - 1),
          ]
          if (nrow(tmp) == 1) {
            path_lon_ws <- path_model$lon[idx() - 1] +
              (path_model$lon[idx()] - path_model$lon[idx() - 1]) *
                Re(tmp$ws / tmp$gs)
            path_lat_ws <- path_model$lat[idx() - 1] +
              (path_model$lat[idx()] - path_model$lat[idx() - 1]) *
                Im(tmp$ws / tmp$gs)
          }
        }
        proxy <- proxy |>
          leaflet::addPolylines(
            lng = c(
              path_model$lon[idx() - 1],
              path_lon_ws,
              path_model$lon[idx()]
            ),
            lat = c(
              path_model$lat[idx() - 1],
              path_lat_ws,
              path_model$lat[idx()]
            ),
            opacity = 1,
            color = "#FFF",
            weight = 3
          ) |>
          leaflet::addCircleMarkers(
            lng = path_model$lon[idx() - 1],
            lat = path_model$lat[idx() - 1],
            fillOpacity = 1,
            fillColor = stap_model$col[idx() - 1],
            weight = 1,
            color = "#FFF",
            radius = stap_model$duration[idx() - 1]^(0.3) * 10
          ) |>
          leaflet::addCircles(
            lng = path_model$lon[idx() - 1],
            lat = path_model$lat[idx() - 1],
            opacity = 1,
            color = stap_model$col[idx() - 1],
            radius = as.numeric(input$speed) * sum(fl_dur[idx() - 1]) * 1000,
            fillOpacity = 0,
            weight = 2
          )
      }
      if (idx() != length(stap_id_include())) {
        # find position from wind only
        # path_model$stap_id[idx() + (0:1)]

        proxy <- proxy |>
          leaflet::addPolylines(
            lng = path_model$lon[idx() + (0:1)],
            lat = path_model$lat[idx() + (0:1)],
            opacity = 1,
            color = "#FFF",
            weight = 3
          ) |>
          leaflet::addCircleMarkers(
            lng = path_model$lon[idx() + 1],
            lat = path_model$lat[idx() + 1],
            fillOpacity = 1,
            fillColor = stap_model$col[idx() + 1],
            weight = 1,
            color = "#FFF",
            radius = stap_model$duration[idx() + 1]^(0.3) * 10
          ) |>
          leaflet::addCircles(
            lng = path_model$lon[idx() + 1],
            lat = path_model$lat[idx() + 1],
            opacity = 1,
            color = stap_model$col[idx() + 1],
            radius = as.numeric(input$speed) * fl_dur[idx()] * 1000,
            fillOpacity = 0,
            weight = 2
          )
      }
      proxy <- proxy |>
        leaflet::addCircleMarkers(
          lng = reactVal$path$lon[as.numeric(input$stap_id)],
          lat = reactVal$path$lat[as.numeric(input$stap_id)],
          opacity = 1,
          fillOpacity = 1,
          radius = stap$duration[as.numeric(input$stap_id)]^(0.3) * 10,
          fillColor = stap$col[as.numeric(input$stap_id)],
          color = "white",
          weight = 2
        )
    }
    proxy
  }) # |> bindEvent(input$stap_id)

  # Helper to post-process and merge pressure time series results
  process_pressuretimeseries <- function(
    pressuretimeseries,
    stap_idx,
    stap_id,
    requested_lat,
    requested_lon,
    requested_at,
    completed_at
  ) {
    # Keep `stap_id` as the identifier used in `pressure$stap_id` so grouping/normalization
    # stays consistent with `geopressure_timeseries()` and existing `pressurepath` objects.
    pressuretimeseries$stap_ref <- stap_id
    pressuretimeseries$col <- stap$col[stap_idx]

    cache_file <- try(
      getFromNamespace("pressure_query_cache_write", "GeoPressureR")(list(
        tag_id = tag$param$id,
        stap_id = stap_id,
        date = pressuretimeseries$date,
        pressure_tag = pressuretimeseries$pressure_tag,
        surface_pressure = pressuretimeseries$surface_pressure,
        requested_lat = requested_lat,
        requested_lon = requested_lon,
        returned_lat = pressuretimeseries$lat[1],
        returned_lon = pressuretimeseries$lon[1],
        requested_at = requested_at,
        completed_at = completed_at
      )),
      silent = TRUE
    )

    # Ensure existing pressurepath has columns we rely on (older saved objects may not).
    if (nrow(reactVal$pressurepath) > 0) {
      if (!"stap_id" %in% names(reactVal$pressurepath)) {
        reactVal$pressurepath$stap_id <- NA
      }
      if (!"stap_ref" %in% names(reactVal$pressurepath)) {
        reactVal$pressurepath$stap_ref <- NA
      }
      if (!"linetype" %in% names(reactVal$pressurepath)) {
        reactVal$pressurepath$linetype <- NA
      }
      if (!"col" %in% names(reactVal$pressurepath)) reactVal$pressurepath$col <- NA
    }

    stap_col <- if ("stap_id" %in% names(reactVal$pressurepath)) {
      "stap_id"
    } else if ("stap_ref" %in% names(reactVal$pressurepath)) {
      "stap_ref"
    } else {
      NULL
    }

    has_prev <- !is.null(stap_col) &&
      any(reactVal$pressurepath[[stap_col]] == stap_id, na.rm = TRUE)

    prev_linetype_max <- 0
    if (isTRUE(has_prev) && "linetype" %in% names(reactVal$pressurepath)) {
      prev_vals <- suppressWarnings(as.numeric(reactVal$pressurepath$linetype[
        reactVal$pressurepath[[stap_col]] == stap_id
      ]))
      prev_vals <- prev_vals[is.finite(prev_vals)]
      if (length(prev_vals) > 0) {
        prev_linetype_max <- max(prev_vals)
      }
    }

    pressuretimeseries$linetype <- as.factor(if (isTRUE(has_prev)) prev_linetype_max + 1 else 1)

    if ("j" %in% names(reactVal$pressurepath)) {
      pressuretimeseries$j <- reactVal$pressurepath$j[1]
    }
    if ("ind" %in% names(reactVal$pressurepath)) {
      pressuretimeseries$ind <- NA
    }
    if (!is.null(stap_col) && "include" %in% names(reactVal$pressurepath)) {
      pressuretimeseries$include <- reactVal$pressurepath$include[
        reactVal$pressurepath[[stap_col]] == stap_id
      ][1]
    }
    if (!is.null(stap_col) && "known" %in% names(reactVal$pressurepath)) {
      pressuretimeseries$known <- reactVal$pressurepath$known[
        reactVal$pressurepath[[stap_col]] == stap_id
      ][1]
    }

    # Update path with potentially corrected lat/lon
    reactVal$path$lon[stap_idx] <- pressuretimeseries$lon[1]
    reactVal$path$lat[stap_idx] <- pressuretimeseries$lat[1]
    if (!"ind" %in% names(reactVal$path)) {
      reactVal$path$ind <- NA_integer_
    }
    reactVal$path$ind[stap_idx] <- latlon2ind(
      pressuretimeseries$lat[1],
      pressuretimeseries$lon[1]
    )

    # Merge new series into shiny::reactive pressurepath, aligning columns
    if (nrow(reactVal$pressurepath) > 0) {
      missing_cols <- setdiff(
        names(reactVal$pressurepath),
        names(pressuretimeseries)
      )
      pressuretimeseries[missing_cols] <- NA
      columns_to_keep <- intersect(
        names(reactVal$pressurepath),
        names(pressuretimeseries)
      )
      pressuretimeseries <- pressuretimeseries[, columns_to_keep]
      reactVal$pressurepath <- rbind(reactVal$pressurepath, pressuretimeseries)
    } else {
      reactVal$pressurepath <- pressuretimeseries
    }

    # Trigger UI refresh on selection
    shiny::updateSelectizeInput(session, "stap_id", selected = "1")
    shiny::updateSelectizeInput(session, "stap_id", selected = input$stap_id)

    invisible(!inherits(cache_file, "try-error"))
  }

  # Async query for "Query pressure" button
  source("server_query_position.R", local = TRUE)
  setup_query_position(
    reactVal = reactVal,
    stap = stap,
    pressure = pressure,
    process_pressuretimeseries = process_pressuretimeseries,
    session = session
  )

  shiny::observeEvent(input$save_path, {
    target_file <- glue::glue("./data/interim/{tag$param$id}-path-geopressureviz.csv")

    tryCatch(
      {
        dir.create("./data/interim", recursive = TRUE, showWarnings = FALSE)
        utils::write.csv(
          reactVal$path,
          file = target_file,
          row.names = FALSE
        )
        shiny::showNotification(
          glue::glue("Path saved to {target_file}"),
          duration = 5,
          type = "message"
        )
      },
      error = function(e) {
        shiny::showNotification(
          glue::glue("Save failed: {e$message}. Using manual download instead."),
          duration = 10,
          type = "warning"
        )
        shinyjs::click("export_path")
      }
    )
  })

  output$export_path <- shiny::downloadHandler(
    filename = function() {
      glue::glue("{tag$param$id}-path-geopressureviz.csv")
    },
    content = function(file) {
      utils::write.csv(
        reactVal$path,
        file = file,
        row.names = FALSE
      )
    }
  )

  # Pressure Graph
  shiny::observe({
    if (!input$full_track) {
      stap_id <- stap$stap_id[as.numeric(input$stap_id)]
      pressure_val_stap_id <- pressure$value[pressure$stap_id == stap_id]
      plotly::plotlyProxy("pressure_plot", session) |>
        plotly::plotlyProxyInvoke(
          "relayout",
          list(
            yaxis = list(
              range = c(
                min(pressure_val_stap_id, na.rm = TRUE) - 5,
                max(pressure_val_stap_id, na.rm = TRUE) + 5
              )
            ),
            xaxis = list(
              range = c(
                stap$start[as.numeric(input$stap_id)] - 60 * 60 * 24,
                stap$end[as.numeric(input$stap_id)] + 60 * 60 * 24
              )
            )
          )
        )
    } else {
      plotly::plotlyProxy("pressure_plot", session) |>
        plotly::plotlyProxyInvoke(
          "relayout",
          list(
            yaxis = list(autorange = TRUE),
            xaxis = list(autorange = TRUE)
          )
        )
    }
  })
}
