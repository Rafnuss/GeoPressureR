# nolint start
server <- function(input, output, session) {

  # Compute resolution of projection

  r <- .maps[[1]]
  g <- map_expand(r$extent, r$scale)
  lonInEPSG3857 <- (g$lon * 20037508.34 / 180)
  latInEPSG3857 <- (log(tan((90 + g$lat) * pi / 360)) / (pi / 180)) * (20037508.34 / 180)
  fac_res_proj <- 4
  res_proj <- c(
    median(diff(lonInEPSG3857)),
    median(abs(diff(latInEPSG3857))) / fac_res_proj
  )

  origin_proj <- c(median(lonInEPSG3857), median(latInEPSG3857))

  session$onSessionEnded(function() {
    stopApp()
  })

  observe({
    .GlobalEnv$geopressureviz_path <- reactVal$path
  })

  ## Reactive variable ----

  reactVal <- reactiveValues(
    path = .path,
    pressurepath = .pressurepath,
    isEdit = FALSE # if editing position
  )

  stap_include <- reactive({
    min_dur_stap <- ifelse(is.na(input$min_dur_stap), 0, as.numeric(input$min_dur_stap))
    which(.stap$duration >= min_dur_stap & .stap$include)
  }) |> bindEvent(input$min_dur_stap)

  flight <- reactive({
    stap2flight(.stap, stap_include())
  }) |> bindEvent(input$min_dur_stap)

  # return the map
  map_display <- reactive({
    if (is.null(input$map_source)) {
      return(NA)
    }
    r <- terra::project(
      rast.map(.maps[[input$map_source]]),
      "epsg:3857",
      method = "near",
      res = res_proj,
      origin = origin_proj
    )

    return(r)
  }) |> bindEvent(input$map_source)

  idx <- reactive({
    which(stap_include() == input$i_stap)
  }) |> bindEvent(input$i_stap)











  ## Render ----
  output$map <- leaflet::renderLeaflet({
    map <- leaflet::leaflet() |>
      leaflet::addProviderTiles("CartoDB.DarkMatterNoLabels", group = "Dark Matter") |>
      # options = providerTileOptions(noWrap = TRUE)
      leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
      leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Topography") |>
      leaflet::addLayersControl(
        baseGroups = c("Dark Matter", "Satellite", "Topography"),
        position = c("topleft")
      )
  })
  output$tag_id <- renderUI({
    return(HTML(glue::glue("<h3 style='margin:0;'>", .tag_id, "</h3>")))
  })

  output$flight_prev_info <- renderUI({
    req(input$i_stap)
    fl_dur <- as.numeric(flight()$duration[flight()$stap_t == input$i_stap])
    if (is.null(fl_dur)) {
      return(HTML(""))
    }
    if (idx() != 1) {
      idx_prev <- stap_include()[idx() - 1]
      dist <- geosphere::distGeo(
        reactVal$path[idx_prev, c("lon", "lat")],
        reactVal$path[as.numeric(input$i_stap), c("lon", "lat")]
      ) / 1000
      HTML(
        "<b>Previous flight:</b><br>",
        as.numeric(input$i_stap) - idx_prev, " flights -",
        round(fl_dur, 1), " hrs<br>",
        round(dist), " km - ",
        round(dist / fl_dur), "km/h"
      )
    } else {
      HTML("")
    }
  })

  output$flight_next_info <- renderUI({
    req(input$i_stap)
    fl_dur <- as.numeric(flight()$duration[flight()$stap_s == input$i_stap])
    if (is.null(fl_dur)) {
      return(HTML(""))
    }
    if (idx() != length(stap_include())) {
      idx_next <- stap_include()[idx() + 1]
      dist <- geosphere::distGeo(
        reactVal$path[idx_next, c("lon", "lat")],
        reactVal$path[as.numeric(input$i_stap), c("lon", "lat")]
      ) / 1000
      HTML(
        "<b>Next flight:</b><br>",
        idx_next - as.numeric(input$i_stap), " flights -",
        round(sum(fl_dur), 1), " hrs<br>",
        round(dist), " km - ",
        round(dist / fl_dur), "km/h"
      )
    } else {
      HTML("")
    }
  })

  output$pressure_plot <- plotly::renderPlotly({
    p <- ggplot2::ggplot() +
      ggplot2::geom_line(data = .pressure, ggplot2::aes(x = date, y = value), colour = "grey") +
      ggplot2::geom_point(
        data = subset(.pressure, label == "discard"),
        ggplot2::aes(x = date, y = value),
        colour = "black"
      ) +
      ggplot2::theme_bw()

    if (nrow(reactVal$pressurepath) > 0) {
      p <- p + ggplot2::geom_line(
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

    plotly::ggplotly(p, dynamicTicks = T, height = 300, tooltip = c("x", "y", "linetype")) |>
      plotly::layout(
        showlegend = F,
        yaxis = list(title = "Pressure [hPa]")
      )
  })









  ## ObserveEvents ----
  # Same order than the ui

  observeEvent(input$full_track, {
    if (input$full_track) {
      shinyjs::hide(id = "stap_info_view", anim = T)
      shinyjs::show(id = "track_info_view", anim = T)
    } else {
      shinyjs::show(id = "stap_info_view", anim = T)
      shinyjs::hide(id = "track_info_view", anim = T)
    }
  })

  observeEvent(input$min_dur_stap, {
    if (length(stap_include()) > 0) {
      choices <- as.list(stap_include())
      names(choices) <-
        glue::glue("#{stap_include()} ({round(.stap$duration[stap_include()], 1)} d.)")
    } else {
      choices <- list()
    }
    updateSelectizeInput(session, "i_stap", choices = choices)
  })

  observeEvent(input$previous_position, {
    idx_new <- min(max(idx() - 1, 1), length(stap_include()))
    updateSelectizeInput(session, "i_stap", selected = stap_include()[idx_new])
  })

  observeEvent(input$next_position, {
    idx_new <- min(max(idx() + 1, 1), length(stap_include()))
    updateSelectizeInput(session, "i_stap", selected = stap_include()[idx_new])
  })

  observeEvent(input$edit_position, {
    if (reactVal$isEdit) {
      reactVal$isEdit <- F
      updateActionButton(session, "edit_position", label = "Start editing")
      removeClass("edit_position", "primary")
    } else {
      reactVal$isEdit <- T
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
      reactVal$path[as.numeric(input$i_stap), c("lon", "lat")] <- c(click$lng, click$lat)
    }
  })

  # Map
  observe({
    proxy <- leaflet::leafletProxy("map") |>
      leaflet::clearShapes() |>
      leaflet::clearImages() |>
      leaflet::clearMarkers()
    stap_model <- .stap[stap_include(), ]
    path_model <- reactVal$path[stap_include(), c("lon", "lat")]
    fl_dur <- as.numeric(flight()$duration)
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
          lng = path_model$lon, lat = path_model$lat, fillOpacity = 1,
          radius = stap_model$duration^(0.3) * 10, weight = 1, color = "#FFF",
          label = glue::glue("#{stap_model$stap_id}, {round(stap_model$duration, 1)} days"),
          fillColor = stap_model$col
        ) |>
        leaflet::fitBounds(min(path_model$lon), min(path_model$lat), max(path_model$lon),
          max(path_model$lat),
          options = list(paddingBottomRight = c(300, 300))
        )
    } else {
      map_i_stap <- map_display()[[as.numeric(input$i_stap)]]
      if (!is.null(map_i_stap)) {
        proxy <- proxy |> leaflet::addRasterImage(map_i_stap,
          opacity = 0.8,
          colors = leaflet::colorNumeric(
            palette = "magma",
            domain = NULL,
            na.color = "#00000000",
            alpha = TRUE
          ),
          project = "false"
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
        proxy <- proxy |>
          leaflet::addPolylines(
            lng = path_model$lon[idx() + (-1:0)],
            lat = path_model$lat[idx() + (-1:0)],
            opacity = 1,
            color = "#FFF",
            weight = 3
          ) |>
          leaflet::addCircleMarkers(
            lng = path_model$lon[idx() - 1],
            lat = path_model$lat[idx() - 1],
            fillOpacity = 1,
            fillColor = stap_model$col[idx() - 1],
            weight = 1, color = "#FFF",
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
      if (idx() != length(stap_include())) {
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
            weight = 1, color = "#FFF",
            radius = stap_model$duration[idx() + 1]^(0.3) * 10
          ) |>
          leaflet::addCircles(
            lng = path_model$lon[idx() + 1],
            lat = path_model$lat[idx() + 1],
            opacity = 1,
            color = stap_model$col[idx() + 1],
            radius = as.numeric(input$speed) * sum(fl_dur[idx()]) * 1000,
            fillOpacity = 0,
            weight = 2
          )
      }
      proxy <- proxy |>
        leaflet::addCircleMarkers(
          lng = reactVal$path$lon[as.numeric(input$i_stap)],
          lat = reactVal$path$lat[as.numeric(input$i_stap)],
          opacity = 1,
          fillOpacity = 1,
          radius = .stap$duration[as.numeric(input$i_stap)]^(0.3) * 10,
          fillColor = .stap$col[as.numeric(input$i_stap)],
          color = "white",
          weight = 2
        )
    }
    proxy
  }) # |> bindEvent(input$i_stap)

  observeEvent(input$query_position, {
    i_stap <- as.numeric(input$i_stap)
    stap_id <- .stap$stap_id[i_stap]

    pressuretimeseries <- geopressure_timeseries(
      reactVal$path$lat[i_stap],
      reactVal$path$lon[i_stap],
      pressure = .pressure[.pressure$stap_id == stap_id, ]
    )

    # Find the new index for linetype
    pressuretimeseries$linetype <- as.factor(ifelse(
      any(reactVal$pressurepath$stap_id == stap_id),
      max(as.numeric(reactVal$pressurepath$linetype[reactVal$pressurepath$stap_id == stap_id])) + 1,
      1
    ))

    pressuretimeseries$stap_ref <- stap_id
    pressuretimeseries$col <- .stap$col[.stap$stap_id == stap_id][1]

    # update lat lon in case over water
    reactVal$path$lon[i_stap] <- pressuretimeseries$lon[1]
    reactVal$path$lat[i_stap] <- pressuretimeseries$lat[1]

    # Merge the two data.frame
    if (nrow(reactVal$pressurepath) > 0) {
      pressuretimeseries <- pressuretimeseries[, match(
        names(reactVal$pressurepath),
        names(pressuretimeseries)
      )]
      reactVal$pressurepath <- rbind(reactVal$pressurepath, pressuretimeseries)
    } else {
      reactVal$pressurepath <- pressuretimeseries
    }


    # ?
    updateSelectizeInput(session, "i_stap", selected = 1)
    updateSelectizeInput(session, "i_stap", selected = input$i_stap)
  })

  # Pressure Graph
  observe({
    if (!input$full_track) {
      stap_id <- .stap$stap_id[as.numeric(input$i_stap)]
      pressure_val_stap_id <- .pressure$value[.pressure$stap_id == stap_id]
      plotly::plotlyProxy("pressure_plot", session) |>
        plotly::plotlyProxyInvoke(
          "relayout",
          list(
            yaxis = list(range = c(min(pressure_val_stap_id) - 5, max(pressure_val_stap_id) + 5)),
            xaxis = list(range = c(
              .stap$start[as.numeric(input$i_stap)] - 60 * 60 * 24,
              .stap$end[as.numeric(input$i_stap)] + 60 * 60 * 24
            ))
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
