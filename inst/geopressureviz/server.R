
server <- function(input, output, session) {

  # Hide selector if light doesn't exist
  if (is.na("light_prob")) {
    shinyjs::hide(id = "map_source_div")
  }

  ## Reactive variable ----

  reactVal <- reactiveValues(
    path = path0, # path
    ts = ts0, # timeserie of pressurer
    isEdit = F # if editing position
  )

  flight_duration <- reactive({
    id <- which(sta$duration >= as.numeric(input$thr_sta))
    flight_duration <- c()
    for (i_f in seq_len(max(0, length(id) - 1))) {
      from_sta_id <- id[i_f]
      to_sta_id <- id[i_f + 1]

      flight_duration[i_f] <- sum(
        do.call(rbind, lapply(
          flight[seq(from_sta_id, to_sta_id)],
          function(x) {
            sum(x$duration)
          }
        ))
      )
    }
    flight_duration
  }) %>% bindEvent(input$thr_sta)


  map_prob <- reactive({
    if (is.null(input$map_source)) {
      return(NA)
    }
    if (length(input$map_source) == 2) {
      static_prob
    } else if (input$map_source == "Pressure") {
      pressure_prob
    } else if (input$map_source == "Light") {
      light_prob
    } else {
      return(NA)
    }
  }) %>% bindEvent(input$map_source)














  ## Render ----
  output$map <- renderLeaflet({
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels,
        options = providerTileOptions(noWrap = TRUE)
      )
  })

  output$fl_prev_info <- renderUI({
    req(input$i_sta)
    fl_dur <- flight_duration()
    if (is.null(fl_dur)) {
      return(HTML(""))
    }
    id <- which(sta$duration >= as.numeric(input$thr_sta))
    sta_id_thr <- sta$sta_id[id]
    i_s_thr <- which(as.numeric(input$i_sta) == sta_id_thr)

    if (i_s_thr != 1) {
      dist <- distGeo(reactVal$path[id[i_s_thr - 1], ], reactVal$path[id[i_s_thr], ]) / 1000
      HTML(
        "<b>Previous flight:</b><br>",
        sta_id_thr[i_s_thr] - sta_id_thr[i_s_thr - 1], " flights -",
        round(fl_dur[i_s_thr - 1]), " hrs<br>",
        round(dist), " km - ",
        round(dist / fl_dur[i_s_thr - 1]), "km/h"
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
    id <- which(sta$duration >= as.numeric(input$thr_sta))
    sta_id_thr <- sta$sta_id[id]
    i_s_thr <- which(as.numeric(input$i_sta) == sta_id_thr)
    if (i_s_thr != length(sta_id_thr)) {
      dist <- geosphere::distGeo(reactVal$path[id[i_s_thr + 1], ], reactVal$path[id[i_s_thr], ]) / 1000
      HTML(
        "<b>Next flight:</b><br>",
        sta_id_thr[i_s_thr + 1] - sta_id_thr[i_s_thr], " flights -",
        round(sum(fl_dur[i_s_thr])), " hrs<br>",
        round(dist), " km - ",
        round(dist / fl_dur[i_s_thr]), "km/h"
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
      sta_th <- sta[ts$sta_id[1] == sta$sta_id, ]
      if (sta_th$duration > as.numeric(input$thr_sta)) {
        p <- p +
          geom_line(data = ts, aes(x = date, y = pressure0), col = sta_th$col, linetype = ts$lt[1])
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
    id <- sta$duration >= as.numeric(input$thr_sta)
    if (sum(id) > 0) {
      tmp <- as.list(sta$sta_id[id])
      names(tmp) <- paste0("#", sta$sta_id[id], " (", round(sta$duration[id], 1), "d.)")
    } else {
      tmp <- list()
    }
    updateSelectizeInput(session, "i_sta", choices = tmp)
  })

  observeEvent(input$prev_pos, {
    id <- which(sta$duration >= as.numeric(input$thr_sta))
    tmp <- sta$sta_id[id]
    i <- which(input$i_sta == tmp)
    i <- min(max(i - 1, 1), length(tmp))
    updateSelectizeInput(session, "i_sta", selected = tmp[i])
  })

  observeEvent(input$next_pos, {
    id <- which(sta$duration >= as.numeric(input$thr_sta))
    tmp <- sta$sta_id[id]
    i <- which(input$i_sta == tmp)
    i <- min(max(i + 1, 1), length(tmp))
    updateSelectizeInput(session, "i_sta", selected = tmp[i])
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
      id <- which(as.numeric(input$i_sta) == sta$sta_id)
      reactVal$path[id, ] <- c(click$lng, click$lat)
    }
  })

  observeEvent(input$query_pos, {
    i_s <- as.numeric(input$i_sta)
    id <- which(i_s == sta$sta_id)
    pam_pressure_sta <- subset(pressure, sta_id == input$i_sta)
    ts <- geopressure_ts(reactVal$path$lon[id], reactVal$path$lat[id],
      pressure = pam_pressure_sta
    )
    ts$sta_id <- input$i_sta
    ts$pressure0 <- ts$pressure - mean(ts$pressure) + mean(pam_pressure_sta$obs[!pam_pressure_sta$isoutliar])
    ts$lt <- sum(input$i_sta == lapply(reactVal$ts, function(x) {
      x$sta_id[1]
    })) + 1
    reactVal$ts[[length(reactVal$ts) + 1]] <- ts
    updateSelectizeInput(session, "i_sta", selected = 1)
    updateSelectizeInput(session, "i_sta", selected = input$i_sta)
  })

  # Map
  observe({
    proxy <- leafletProxy("map") %>%
      clearShapes() %>%
      clearImages()
    req(input$i_sta)
    id <- which(sta$duration >= as.numeric(input$thr_sta))
    sta_thr <- sta[id, ]
    path_thr <- reactVal$path[id, ]
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
      i_s <- which(as.numeric(input$i_sta) == sta$sta_id)
      i_s_thr <- which(as.numeric(input$i_sta) == sta_thr$sta_id)
      tmp <- map_prob()
      if (any(!is.na(tmp))) {
        proxy <- proxy %>% addRasterImage(tmp[[i_s]], opacity = 0.8, colors = "magma")
      }
      proxy <- proxy %>%
        addPolylines(lng = path_thr$lon, lat = path_thr$lat, opacity = .1, color = "#FFF", weight = 3) %>%
        addPolylines(lng = path_thr$lon[i_s_thr + (-1:1)], lat = path_thr$lat[i_s_thr + (-1:1)], opacity = 1, color = "#FFF", weight = 3) %>%
        addCircles(
          lng = path_thr$lon, lat = path_thr$lat, opacity = .1, color = "#FFF",
          weight = sta_thr$duration^(0.3) * 10
        ) %>%
        addCircles(
          lng = path_thr$lon[i_s_thr], lat = path_thr$lat[i_s_thr], opacity = 1,
          weight = sta_thr$duration[i_s_thr]^(0.3) * 10, color = sta_thr$col[i_s_thr]
        )

      if (i_s != 1) {
        proxy <- proxy %>%
          addCircles(lng = path_thr$lon[i_s_thr - 1], lat = path_thr$lat[i_s_thr - 1], opacity = 1, color = sta_thr$col[i_s_thr - 1], weight = sta_thr$duration[i_s_thr - 1]^(0.3) * 10) %>%
          addCircles(lng = path_thr$lon[i_s_thr - 1], lat = path_thr$lat[i_s_thr - 1], opacity = 1, color = sta_thr$col[i_s_thr - 1], radius = as.numeric(input$speed) * sum(fl_dur[i_s_thr - 1]) * 1000, fillOpacity = 0, weight = 2)
      }
      if (i_s != length(static_prob)) {
        proxy <- proxy %>%
          addCircles(lng = path_thr$lon[i_s_thr + 1], lat = path_thr$lat[i_s_thr + 1], opacity = 1, color = sta_thr$col[i_s_thr + 1], weight = sta_thr$duration[i_s_thr]^(0.3) * 10) %>%
          addCircles(lng = path_thr$lon[i_s_thr + 1], lat = path_thr$lat[i_s_thr + 1], opacity = 1, color = sta_thr$col[i_s_thr + 1], radius = as.numeric(input$speed) * sum(fl_dur[i_s_thr]) * 1000, fillOpacity = 0, weight = 2)
      }
    }
    proxy
  }) # %>% bindEvent(input$i_sta)

  # Pressure Graph
  observe({
    if (!input$allsta) {
      i_s <- as.numeric(input$i_sta)
      i_sta <- i_s == sta$sta_id
      id <- pressure$sta_id == i_s
      plotlyProxy("pressure_graph", session) %>%
        plotlyProxyInvoke(
          "relayout",
          list(
            yaxis = list(range = c(min(pressure$obs[id]) - 5, max(pressure$obs[id]) + 5)),
            xaxis = list(range = c(sta$start[i_sta] - 60 * 60 * 24, sta$end[i_sta] + 60 * 60 * 24))
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
