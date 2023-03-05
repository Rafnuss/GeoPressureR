server <- function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })

  ## Reactive variable ----

  reactVal <- reactiveValues(
    path = .path0, # path
    ts = .ts0, # timeserie of pressurer
    isEdit = F # if editing position
  )

  stap_include <- reactive({
    which(.stap$duration >= as.numeric(input$min_dur_stap) & .stap$include)
  }) %>% bindEvent(input$min_dur_stap)

  flight <- reactive({
    stap2flight(.stap, stap_include())
  }) %>% bindEvent(stap_include)

  # return the map
  map_display <- reactive({
    if (is.null(input$map_source)) {
      return(NA)
    }
    return(.maps[[input$map_source]])
  }) %>% bindEvent(input$map_source)

  idx <- reactive({
    which(stap_include() == input$i_stap)
  }) %>% bindEvent(input$i_stap)











  ## Render ----
  output$map <- renderLeaflet({
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Dark Matter") %>% # options = providerTileOptions(noWrap = TRUE)
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topography") %>%
      addLayersControl(baseGroups = c("Dark Matter", "Satellite", "Topography"), position = c("topleft"))
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
      dist <- distGeo(reactVal$path[idx_prev, c("lon", "lat")], reactVal$path[as.numeric(input$i_stap), c("lon", "lat")]) / 1000
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
      dist <- geosphere::distGeo(reactVal$path[idx_next, c("lon", "lat")], reactVal$path[as.numeric(input$i_stap), c("lon", "lat")]) / 1000
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

  output$pressure_plot <- renderPlotly({
    p <- ggplot() +
      geom_line(data = .pressure, aes(x = date, y = value), colour = "grey") +
      geom_point(data = subset(.pressure, label=="discard"), aes(x = date, y = value), colour = "black") +
      theme_bw()

    req(input$min_dur_stap)
    for (ts in reactVal$ts) {
      sta_th <- .stap[median(ts$sta_id, na.rm = TRUE) == .stap$sta_id, ]
      if (nrow(sta_th) > 0) {
        if (sta_th$duration > as.numeric(input$min_dur_stap)) {
          p <- p +
            geom_line(data = ts, aes(x = date, y = pressure0), col = sta_th$col, linetype = ts$lt[1])
        }
      }
    }

    ggplotly(p, dynamicTicks = T, height = 300, tooltip = c("date", "pressure0", "lt")) %>%
      layout(
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
      names(choices) <- glue::glue("#{stap_include()} ({round(.stap$duration[stap_include()], 1)} d.)")
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
    print(click)
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
    proxy <- leafletProxy("map") %>%
      clearShapes() %>%
      clearImages()
    stap_model <- .stap[stap_include(), ]
    path_model <- reactVal$path[stap_include(), c("lon", "lat")]
    fl_dur <- as.numeric(flight()$duration)
    if (is.null(fl_dur)) {
      return()
    }
    if (input$full_track) {
      proxy <- proxy %>%
        addPolylines(lng = path_model$lon, lat = path_model$lat, opacity = 1, color = "#FFF", weight = 3) %>%
        addCircles(
          lng = path_model$lon, lat = path_model$lat, opacity = 1, weight = stap_model$duration^(0.3) * 10,
          label = glue::glue("#{stap_model$stap_id}, {round(stap_model$duration, 1)} days"), color = stap_model$col
        ) %>%
        fitBounds(min(path_model$lon), min(path_model$lat), max(path_model$lon), max(path_model$lat), options = list(paddingBottomRight = c(300, 300)))
    } else {
      map_i_stap <- map_display()[[as.numeric(input$i_stap)]]
      if (any(!is.null(map_i_stap))) {
        rast <- raster(map_i_stap, xmn=.extent[1], xmx=.extent[2], ymn=.extent[3], ymx=.extent[4],
                       crs= "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
        proxy <- proxy %>% addRasterImage(rast, opacity = 0.8, colors = "magma", method="ngb")
      }

      proxy <- proxy %>%
        addPolylines(lng = path_model$lon, lat = path_model$lat, opacity = .1, color = "#FFF", weight = 3) %>%
        addCircles(
          lng = path_model$lon, lat = path_model$lat, opacity = .1, color = "#FFF",
          weight = stap_model$duration^(0.3) * 10
        )

      # Index in sta_short

      if (idx() != 1) {
        proxy <- proxy %>%
          addPolylines(lng = path_model$lon[idx() + (-1:0)],
                       lat = path_model$lat[idx() + (-1:0)],
                       opacity = 1,
                       color = "#FFF",
                       weight = 3) %>%
          addCircles(lng = path_model$lon[idx() - 1],
                     lat = path_model$lat[idx() - 1],
                     opacity = 1,
                     color = stap_model$col[idx() - 1],
                     weight = stap_model$duration[idx() - 1]^(0.3) * 10) %>%
          addCircles(lng = path_model$lon[idx() - 1],
                     lat = path_model$lat[idx() - 1],
                     opacity = 1,
                     color = stap_model$col[idx() - 1],
                     radius = as.numeric(input$speed) * sum(fl_dur[idx() - 1]) * 1000,
                     fillOpacity = 0,
                     weight = 2)
      }
      if (idx() != length(stap_include())) {
        proxy <- proxy %>%
          addPolylines(lng = path_model$lon[idx() + (0:1)],
                       lat = path_model$lat[idx() + (0:1)],
                       opacity = 1,
                       color = "#FFF",
                       weight = 3) %>%
          addCircles(lng = path_model$lon[idx() + 1],
                     lat = path_model$lat[idx() + 1],
                     opacity = 1,
                     color = stap_model$col[idx() + 1],
                     weight = stap_model$duration[idx() + 1]^(0.3) * 10) %>%
          addCircles(lng = path_model$lon[idx() + 1],
                     lat = path_model$lat[idx() + 1],
                     opacity = 1,
                     color = stap_model$col[idx() + 1],
                     radius = as.numeric(input$speed) * sum(fl_dur[idx()]) * 1000,
                     fillOpacity = 0,
                     weight = 2)
      }
      proxy <- proxy %>%
        addCircles(
          lng = reactVal$path$lon[as.numeric(input$i_stap)],
          lat = reactVal$path$lat[as.numeric(input$i_stap)],
          opacity = 1,
          weight = .stap$duration[as.numeric(input$i_stap)]^(0.3) * 10,
          color = .stap$col[as.numeric(input$i_stap)]
        )
    }
    proxy
  }) # %>% bindEvent(input$i_stap)

  observeEvent(input$query_position, {
    sta_id <- .stap$sta_id[as.numeric(input$i_stap)]
    pam_pressure_sta <- .pressure[.pressure$sta_id == sta_id, ]
    ts <- geopressure_timeseries_latlon(
      reactVal$path$lat[as.numeric(input$i_stap)],
      reactVal$path$lon[as.numeric(input$i_stap)],
      pressure = pam_pressure_sta
    )
    ts$lt <- sum(sta_id == unlist(lapply(reactVal$ts, function(x) {
      x$sta_id[1]
    }))) + 1
    reactVal$path$lon[as.numeric(input$i_stap)] <- ts$lon[1]
    reactVal$path$lat[as.numeric(input$i_stap)] <- ts$lat[1]
    reactVal$ts[[length(reactVal$ts) + 1]] <- ts
    updateSelectizeInput(session, "i_stap", selected = 1)
    updateSelectizeInput(session, "i_stap", selected = input$i_stap)
  })

  # Pressure Graph
  observe({
    if (!input$full_track) {
      sta_id <- .stap$sta_id[as.numeric(input$i_stap)]
      pres_sta_id <- .pressure$sta_id == sta_id
      plotlyProxy("pressure_plot", session) %>%
        plotlyProxyInvoke(
          "relayout",
          list(
            yaxis = list(range = c(min(.pressure$value[pres_sta_id]) - 5, max(.pressure$value[pres_sta_id]) + 5)),
            xaxis = list(range = c(.stap$start[as.numeric(input$i_stap)] - 60 * 60 * 24, .stap$end[as.numeric(input$i_stap)] + 60 * 60 * 24))
          )
        )
    } else {
      plotlyProxy("pressure_plot", session) %>%
        plotlyProxyInvoke(
          "relayout",
          list(
            yaxis = list(autorange = T),
            xaxis = list(autorange = T)
          )
        )
    }
  })
}
