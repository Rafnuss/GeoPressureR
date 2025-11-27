# nolint start
server <- function(input, output, session) {
  # Update browser tab title with tag ID
  updateTabsetPanel(session, inputId = NULL)
  session$sendCustomMessage(
    "updateTitle",
    glue::glue("GeoPressureViz - {tag$param$id}")
  )

  session$onSessionEnded(function() {
    stopApp()
  })

  # Extract shorter name for variable
  stap <- tag$stap
  pressure <- tag$pressure

  if (is.null(file_wind)) {
    edge <- NULL
  } else {
    edge <- path2edge(path, tag)

    uv <- edge_add_wind(
      tag,
      edge_s = edge$s,
      edge_t = edge$t,
      return_averaged_variable = TRUE,
      file = file_wind
    )
    edge$ws <- (uv[, 1] + 1i * uv[, 2]) / 1000 * 60 * 60
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

  # Initialize future plan for async tasks (once per session)
  if (isTRUE(future::nbrOfWorkers() <= 1)) {
    try(
      {
        if (future::supportsMultisession()) future::plan(future::multisession)
      },
      silent = TRUE
    )
  }

  observe({
    # Store current path as path_geopressureviz in shiny options
    shiny::shinyOptions(path_geopressureviz = reactVal$path)
  })

  ## Reactive variable ----

  reactVal <- reactiveValues(
    path = path,
    edge = edge,
    pressurepath = pressurepath,
    isEdit = FALSE # if editing position
  )

  # return the map
  map_display <- reactive({
    if (is.null(input$map_source)) {
      return(NA)
    }
    r <- rast.map(maps[[input$map_source]])
    r_norm <- (r - terra::minmax(r)[1]) / diff(terra::minmax(r))
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
  stap_id_include <- reactive({
    min_dur_stap <- ifelse(
      is.na(input$min_dur_stap),
      0,
      as.numeric(input$min_dur_stap)
    )
    which(stap$duration >= min_dur_stap & stap$include)
  }) |>
    bindEvent(input$min_dur_stap)

  # Precompute flight durations for current included stap ids
  flight_dur_reactive <- reactive({
    stap2flight(stap, stap_id_include())$duration
  }) |>
    bindEvent(stap_id_include())

  # index of the current stap_id in the stap_id_include (so not index in all stap_id, only the one to use)
  idx <- reactive({
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
  output$tag_id <- renderUI({
    return(HTML(glue::glue("<h3 style='margin:0;'>", tag$param$id, "</h3>")))
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

    speed_txt <- if (!is.na(fl_dur) && fl_dur > 0) {
      paste0(round(dist_km / fl_dur), "km/h")
    } else {
      "—"
    }

    label <- if (nextprev > 0) "Next flight" else "Previous flight"

    return(HTML(
      glue::glue("<b>{label}:</b><br>"),
      glue::glue(
        "{nb_fl} flights - {round(fl_dur, 1)} hrs<br>"
      ),
      glue::glue("{round(dist_km)} km - {speed_txt}")
    ))
  }

  output$flight_prev_info <- renderUI({
    req(input$stap_id)
    if (idx() == 1) {
      return(HTML(""))
    }
    flight_info(idx(), -1)
  })

  output$flight_next_info <- renderUI({
    req(input$stap_id)
    if (idx() == length(stap_id_include())) {
      return(HTML(""))
    }
    flight_info(idx(), +1)
  })

  output$pressure_plot <- plotly::renderPlotly({
    p <- ggplot2::ggplot() +
      ggplot2::geom_line(
        data = pressure,
        ggplot2::aes(x = date, y = value),
        colour = "grey"
      ) +
      ggplot2::geom_point(
        data = subset(pressure, label == "discard"),
        ggplot2::aes(x = date, y = value),
        colour = "black"
      ) +
      ggplot2::theme_bw()

    if (nrow(reactVal$pressurepath) > 0) {
      p <- p +
        ggplot2::geom_line(
          data = reactVal$pressurepath,
          ggplot2::aes(
            x = date,
            y = surface_pressure_norm,
            color = col,
            group = stap_id,
            linetype = linetype
          )
        ) +
        ggplot2::scale_color_identity()
    }

    plotly::ggplotly(
      p,
      dynamicTicks = TRUE,
      height = 300,
      tooltip = c("x", "y", "linetype")
    ) |>
      plotly::layout(
        showlegend = FALSE,
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

  ## ObserveEvents ----
  # Same order than the ui

  observeEvent(input$full_track, {
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

  observeEvent(input$min_dur_stap, {
    if (length(stap_id_include()) > 0) {
      choices <- as.list(stap_id_include())
      names(choices) <-
        glue::glue(
          "#{stap_id_include()} ({round(stap$duration[stap_id_include()], 1)} d.)"
        )
    } else {
      choices <- list()
    }
    session$onFlushed(function() {
      updateSelectInput(session, "stap_id", choices = choices)
    })
  })

  observeEvent(input$previous_position, {
    idx_new <- min(max(idx() - 1, 1), length(stap_id_include()))
    updateSelectInput(session, "stap_id", selected = stap_id_include()[idx_new])
  })

  observeEvent(input$next_position, {
    idx_new <- min(max(idx() + 1, 1), length(stap_id_include()))
    updateSelectInput(session, "stap_id", selected = stap_id_include()[idx_new])
  })

  observeEvent(input$edit_position, {
    if (reactVal$isEdit) {
      reactVal$isEdit <- FALSE
      updateActionButton(session, "edit_position", label = "Start editing")
      removeClass("edit_position", "primary")
    } else {
      reactVal$isEdit <- TRUE
      updateActionButton(session, "edit_position", label = "Stop editing")
      addClass("edit_position", "primary")
    }
  })

  observeEvent(input$map_click, {
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
  observe({
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
        proxy <- proxy |>
          leaflet::addRasterImage(
            map_stap_id,
            opacity = 0.8,
            colors = leaflet::colorNumeric(
              palette = "magma",
              domain = NULL,
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
          opacity = .1,
          color = "#FFF",
          weight = 3
        ) |>
        leaflet::addCircles(
          lng = path_model$lon,
          lat = path_model$lat,
          fillOpacity = .1,
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
  process_pressuretimeseries <- function(pressuretimeseries, stap_id) {
    # Compute new linetype index
    pressuretimeseries$linetype <- as.factor(ifelse(
      any(reactVal$pressurepath$stap_id == stap_id),
      max(as.numeric(reactVal$pressurepath$linetype[
        reactVal$pressurepath$stap_id == stap_id
      ])) +
        1,
      1
    ))

    pressuretimeseries$stap_ref <- stap_id
    pressuretimeseries$col <- stap$col[stap$stap_id == stap_id][1]

    if ("j" %in% names(reactVal$pressurepath)) {
      pressuretimeseries$j <- reactVal$pressurepath$j[1]
    }
    if ("ind" %in% names(reactVal$pressurepath)) {
      pressuretimeseries$ind <- NA
    }
    if ("include" %in% names(reactVal$pressurepath)) {
      pressuretimeseries$include <- reactVal$pressurepath$include[
        reactVal$pressurepath$stap_id == stap_id
      ][1]
    }
    if ("known" %in% names(reactVal$pressurepath)) {
      pressuretimeseries$known <- reactVal$pressurepath$known[
        reactVal$pressurepath$stap_id == stap_id
      ][1]
    }

    # Update path with potentially corrected lat/lon
    reactVal$path$lon[stap_id] <- pressuretimeseries$lon[1]
    reactVal$path$lat[stap_id] <- pressuretimeseries$lat[1]
    reactVal$path$ind[stap_id] <- latlon2ind(
      pressuretimeseries$lat[1],
      pressuretimeseries$lon[1]
    )

    # Merge new series into reactive pressurepath, aligning columns
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
    updateSelectInput(session, "stap_id", selected = 1)
    updateSelectInput(session, "stap_id", selected = input$stap_id)

    invisible(NULL)
  }

  observeEvent(input$query_position, {
    # Prepare inputs outside of the async call to avoid capturing large/reactive objects
    stap_idx <- as.numeric(input$stap_id)
    stap_id <- stap$stap_id[stap_idx]
    lat0 <- reactVal$path$lat[stap_id]
    lon0 <- reactVal$path$lon[stap_id]
    pres_df <- pressure[pressure$stap_id == stap_id, ]
    # Prevent repeat clicks while running
    try(shinyjs::disable("query_position"), silent = TRUE)

    if (!requireNamespace("promises", quietly = TRUE)) {
      # Fallback: synchronous execution if promises is unavailable
      tryCatch(
        {
          pressuretimeseries <- geopressure_timeseries(
            lat0,
            lon0,
            pressure = pres_df
          )
          process_pressuretimeseries(pressuretimeseries, stap_id)
        },
        error = function(e) {
          cli::cli_alert_warning(c(
            "!" = "Function {.fun geopressure_timeseries} did not work.",
            "i" = conditionMessage(e)
          ))
        }
      )
      try(shinyjs::enable("query_position"), silent = TRUE)
      return(invisible())
    }

    # Async path: run geopressure_timeseries in a background R session and return a promise
    promises::future_promise({
      geopressure_timeseries(lat0, lon0, pressure = pres_df)
    }) |>
      promises::then(function(pressuretimeseries) {
        process_pressuretimeseries(pressuretimeseries, stap_id)
        try(shinyjs::enable("query_position"), silent = TRUE)
        invisible(NULL)
      }) |>
      promises::catch(function(e) {
        cli::cli_alert_warning(c(
          "!" = "Function {.fun geopressure_timeseries} did not work.",
          "i" = conditionMessage(e)
        ))
        try(shinyjs::enable("query_position"), silent = TRUE)
        invisible(NULL)
      }) |>
      invisible()
  })

  # Export path functionality
  observeEvent(input$export_path, {
    file <- glue::glue("./data/interim/{tag$param$id}.RData")

    # Create directory if it doesn't exist
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)

    # Rename to the correct name
    path_geopressureviz <- reactVal$path

    saved_ok <- tryCatch(
      {
        if (file.exists(file)) {
          # Load existing file into a dedicated env and add path_geopressureviz
          env <- new.env(parent = emptyenv())
          save_list <- load(file, envir = env)
          env$path_geopressureviz <- path_geopressureviz
          save_list <- unique(c(save_list, "path_geopressureviz"))
          save(
            list = save_list,
            envir = env,
            file = file
          )
        } else {
          # Create new file with just path_geopressureviz
          save(
            path_geopressureviz,
            file = file
          )
        }
        TRUE
      },
      error = function(e) {
        showNotification(
          paste("Failed to export path:", conditionMessage(e)),
          type = "error",
          duration = 5
        )
        FALSE
      }
    )

    if (saved_ok) {
      showNotification(
        paste("Path exported to", normalizePath(file)),
        type = "message",
        duration = 3
      )
    }
  })

  # Pressure Graph
  observe({
    if (!input$full_track) {
      stap_id <- stap$stap_id[as.numeric(input$stap_id)]
      pressure_val_stap_id <- pressure$value[pressure$stap_id == stap_id]
      plotly::plotlyProxy("pressure_plot", session) |>
        plotly::plotlyProxyInvoke(
          "relayout",
          list(
            yaxis = list(
              range = c(
                min(pressure_val_stap_id) - 5,
                max(pressure_val_stap_id) + 5
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
# nolint end
