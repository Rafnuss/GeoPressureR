server <- function(input, output, session) {

  ## Reactive variable ----

  reactVal <- reactiveValues(
    path = path0, # path
    ts = ts0, # timeserie of pressurer
    isEdit = F # if editing position
  )

  flight_duration <- reactive({
    idx_sta_short <- which(sta$duration >= as.numeric(input$thr_sta))
    flight_duration <- c()
    for (i_f in seq_len(max(0, length(idx_sta_short) - 1))) {
      from_idx_sta_short <- idx_sta_short[i_f]
      to_idx_sta_short <- idx_sta_short[i_f + 1] - 1

      flight_duration[i_f] <- sum(
        do.call(rbind, lapply(
          flight[seq(from_idx_sta_short, to_idx_sta_short)],
          function(x) {
            sum(x$duration)
          }
        ))
      )
    }
    flight_duration
  }) %>% bindEvent(input$thr_sta)

  # return the map
  map_prob <- reactive({
    if (is.null(input$map_source)) {
      return(NA)
    }
    return(map_val[[which(input$map_source == map_choices)]])
  }) %>% bindEvent(input$map_source)














  ## Render ----
  output$map <- renderLeaflet({
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Dark Matter") %>% # options = providerTileOptions(noWrap = TRUE)
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topography") %>%
      addLayersControl(baseGroups = c("Dark Matter", "Satellite", "Topography"), position = c("topleft"))
  })
  output$gdl_id <- renderUI({
    return(HTML(paste0("<h3 style='margin:0;'>", gdl_id, "</h3>")))
  })

  output$fl_prev_info <- renderUI({
    req(input$i_sta)
    fl_dur <- flight_duration()
    if (is.null(fl_dur)) {
      return(HTML(""))
    }
    idx_sta_short <- which(sta$duration >= as.numeric(input$thr_sta))
    i <- which(idx_sta_short == as.numeric(input$i_sta))
    if (i != 1) {
      idx_sta_prev <- idx_sta_short[i - 1]
      dist <- distGeo(reactVal$path[idx_sta_prev, ], reactVal$path[as.numeric(input$i_sta), ]) / 1000
      HTML(
        "<b>Previous flight:</b><br>",
        as.numeric(input$i_sta) - idx_sta_prev, " flights -",
        round(fl_dur[i - 1], 1), " hrs<br>",
        round(dist), " km - ",
        round(dist / fl_dur[i - 1]), "km/h"
      )
    } else {
      HTML("")
    }
  })

  output$fl_next_info <- renderUI({
    req(input$i_sta)
    fl_dur <- flight_duration()
    if (is.null(fl_dur)) {
      return(HTML(""))
    }
    idx_sta_short <- which(sta$duration >= as.numeric(input$thr_sta))
    i <- which(idx_sta_short == input$i_sta)
    if (i != length(idx_sta_short)) {
      idx_sta_next <- idx_sta_short[i + 1]
      dist <- geosphere::distGeo(reactVal$path[idx_sta_next, ], reactVal$path[as.numeric(input$i_sta), ]) / 1000
      HTML(
        "<b>Next flight:</b><br>",
        idx_sta_next - as.numeric(input$i_sta), " flights -",
        round(sum(fl_dur[i]), 1), " hrs<br>",
        round(dist), " km - ",
        round(dist / fl_dur[i]), "km/h"
      )
    } else {
      HTML("")
    }
  })

  output$pressure_graph <- renderPlotly({
    p <- ggplot() +
      geom_line(data = pressure, aes(x = date, y = obs), colour = "grey") +
      geom_point(data = subset(pressure, isoutliar), aes(x = date, y = obs), colour = "black") +
      theme_bw()

    req(input$thr_sta)
    for (ts in reactVal$ts) {
      sta_th <- sta[median(ts$sta_id) == sta$sta_id, ]
      if (nrow(sta_th) > 0) {
        if (sta_th$duration > as.numeric(input$thr_sta)) {
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

  observeEvent(input$allsta, {
    if (input$allsta) {
      shinyjs::hide(id = "sta_div", anim = T)
      shinyjs::show(id = "thr_sta_page", anim = T)
    } else {
      shinyjs::show(id = "sta_div", anim = T)
      shinyjs::hide(id = "thr_sta_page", anim = T)
    }
  })

  observeEvent(input$thr_sta, {
    idx_sta_short <- which(sta$duration >= as.numeric(input$thr_sta))
    if (length(idx_sta_short) > 0) {
      tmp <- as.list(idx_sta_short)
      names(tmp) <- paste0("#", sta$sta_id[idx_sta_short], " (", round(sta$duration[idx_sta_short], 1), "d.)")
    } else {
      tmp <- list()
    }
    updateSelectizeInput(session, "i_sta", choices = tmp)
  })

  observeEvent(input$prev_pos, {
    idx_sta_short <- which(sta$duration >= as.numeric(input$thr_sta))
    i <- which(input$i_sta == idx_sta_short)
    i <- min(max(i - 1, 1), length(idx_sta_short))
    updateSelectizeInput(session, "i_sta", selected = idx_sta_short[i])
  })

  observeEvent(input$next_pos, {
    idx_sta_short <- which(sta$duration >= as.numeric(input$thr_sta))
    i <- which(input$i_sta == idx_sta_short)
    i <- min(max(i + 1, 1), length(idx_sta_short))
    updateSelectizeInput(session, "i_sta", selected = idx_sta_short[i])
  })

  observeEvent(input$edit_pos, {
    if (reactVal$isEdit) {
      reactVal$isEdit <- F
      updateActionButton(session, "edit_pos", label = "Start editing")
      removeClass("edit_pos", "primary")
    } else {
      reactVal$isEdit <- T
      updateActionButton(session, "edit_pos", label = "Stop editing")
      addClass("edit_pos", "primary")
    }
  })

  observeEvent(input$map_click, {
    if (is.null(click)) {
      return()
    }
    click <- input$map_click
    if (!reactVal$isEdit) {
      return()
    }
    if (!input$allsta) {
      reactVal$path[as.numeric(input$i_sta), ] <- c(click$lng, click$lat)
    }
  })

  # Map
  observe({
    proxy <- leafletProxy("map") %>%
      clearShapes() %>%
      clearImages()
    req(input$i_sta)
    idx_sta_short <- which(sta$duration >= as.numeric(input$thr_sta))
    sta_thr <- sta[idx_sta_short, ]
    path_thr <- reactVal$path[idx_sta_short, ]
    fl_dur <- flight_duration()
    if (is.null(fl_dur)) {
      return()
    }
    if (input$allsta) {
      proxy <- proxy %>%
        addPolylines(lng = path_thr$lon, lat = path_thr$lat, opacity = 1, color = "#FFF", weight = 3) %>%
        addCircles(
          lng = path_thr$lon, lat = path_thr$lat, opacity = 1, weight = sta_thr$duration^(0.3) * 10,
          label = paste0("#", sta_thr$sta_id, ", ", round(sta_thr$duration, 1), " days"), color = sta_thr$col
        ) %>%
        fitBounds(min(path_thr$lon), min(path_thr$lat), max(path_thr$lon), max(path_thr$lat), options = list(paddingBottomRight = c(300, 300)))
    } else {
      tmp <- map_prob()
      if (any(!is.na(tmp))) {
        proxy <- proxy %>% addRasterImage(tmp[[as.numeric(input$i_sta)]], opacity = 0.8, colors = "magma")
      }

      proxy <- proxy %>%
        addPolylines(lng = path_thr$lon, lat = path_thr$lat, opacity = .1, color = "#FFF", weight = 3) %>%
        addCircles(
          lng = path_thr$lon, lat = path_thr$lat, opacity = .1, color = "#FFF",
          weight = sta_thr$duration^(0.3) * 10
        )

      # Index in sta_short
      i <- which(idx_sta_short == input$i_sta)

      if (i != 1) {
        proxy <- proxy %>%
          addPolylines(lng = path_thr$lon[i + (-1:0)], lat = path_thr$lat[i + (-1:0)], opacity = 1, color = "#FFF", weight = 3) %>%
          addCircles(lng = path_thr$lon[i - 1], lat = path_thr$lat[i - 1], opacity = 1, color = sta_thr$col[i - 1], weight = sta_thr$duration[i - 1]^(0.3) * 10) %>%
          addCircles(lng = path_thr$lon[i - 1], lat = path_thr$lat[i - 1], opacity = 1, color = sta_thr$col[i - 1], radius = as.numeric(input$speed) * sum(fl_dur[i - 1]) * 1000, fillOpacity = 0, weight = 2)
      }
      if (i != length(static_prob)) {
        proxy <- proxy %>%
          addPolylines(lng = path_thr$lon[i + (0:1)], lat = path_thr$lat[i + (0:1)], opacity = 1, color = "#FFF", weight = 3) %>%
          addCircles(lng = path_thr$lon[i + 1], lat = path_thr$lat[i + 1], opacity = 1, color = sta_thr$col[i + 1], weight = sta_thr$duration[i + 1]^(0.3) * 10) %>%
          addCircles(lng = path_thr$lon[i + 1], lat = path_thr$lat[i + 1], opacity = 1, color = sta_thr$col[i + 1], radius = as.numeric(input$speed) * sum(fl_dur[i]) * 1000, fillOpacity = 0, weight = 2)
      }
      proxy <- proxy %>%
        addCircles(
          lng = reactVal$path$lon[as.numeric(input$i_sta)], lat = reactVal$path$lat[as.numeric(input$i_sta)], opacity = 1,
          weight = sta$duration[as.numeric(input$i_sta)]^(0.3) * 10, color = sta$col[as.numeric(input$i_sta)]
        )
    }
    proxy
  }) # %>% bindEvent(input$i_sta)

  observeEvent(input$query_pos, {
    sta_id <- sta$sta_id[as.numeric(input$i_sta)]
    pam_pressure_sta <- pressure[pressure$sta_id == sta_id, ]
    ts <- geopressure_ts(reactVal$path$lon[as.numeric(input$i_sta)], reactVal$path$lat[as.numeric(input$i_sta)],
      pressure = pam_pressure_sta
    )
    ts$lt <- sum(sta_id == unlist(lapply(reactVal$ts, function(x) {
      x$sta_id[1]
    }))) + 1
    reactVal$path$lon[as.numeric(input$i_sta)] <- ts$lon[1]
    reactVal$path$lat[as.numeric(input$i_sta)] <- ts$lat[1]
    reactVal$ts[[length(reactVal$ts) + 1]] <- ts
    updateSelectizeInput(session, "i_sta", selected = 1)
    updateSelectizeInput(session, "i_sta", selected = input$i_sta)
  })

  # Pressure Graph
  observe({
    if (!input$allsta) {
      sta_id <- sta$sta_id[as.numeric(input$i_sta)]
      pres_sta_id <- pressure$sta_id == sta_id
      plotlyProxy("pressure_graph", session) %>%
        plotlyProxyInvoke(
          "relayout",
          list(
            yaxis = list(range = c(min(pressure$obs[pres_sta_id]) - 5, max(pressure$obs[pres_sta_id]) + 5)),
            xaxis = list(range = c(sta$start[as.numeric(input$i_sta)] - 60 * 60 * 24, sta$end[as.numeric(input$i_sta)] + 60 * 60 * 24))
          )
        )
    } else {
      plotlyProxy("pressure_graph", session) %>%
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
