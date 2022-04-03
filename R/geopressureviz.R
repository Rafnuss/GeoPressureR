library(plotly)
library(shinyjs)
library(GeoPressureR)
library(shiny)
library(leaflet)
library(plotly)
library(leaflet.extras)
library(raster)
library(shinyWidgets)

ui <- bootstrapPage(
  useShinyjs(),
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css?family=Oswald", rel = "stylesheet"),
    tags$style(type = "text/css", "html, body {width:100%;height:100%; font-family: Oswald, sans-serif;}.primary{background-color:#007bff; color: #fff;}"),
    # includeHTML("meta.html"),
  ),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(
    top = 0, right = 0, draggable = F, width = "200px", style = "z-index:500; min-width: 300px;background-color:white;padding: 5px 10px;",
    tags$h2("GeoPressureViz", style = "text-align: right;"),
    tags$a("About GeoPressureR", href = "https://raphaelnussbaumer.com/GeoPressureR/", style = "text-align: right;display: block;padding-bottom:20px;"),
    div(
      style = "text-align: center;",
      "Full Track",
      switchInput("allsta", value = TRUE, inline = T, size = "small")
    ),
    fluidPage(
      id = "thr_sta_page",
      fluidRow(
        column(8, tags$p("Minimum duration [days]", style = "font-weight:bold; line-height: 34px;text-align: right;"), ),
        column(4, style = "padding:0px;", numericInput("thr_sta", NULL, min = 0, max = 50, value = 1, step = 0.5), ),
      )
    ),
    div(
      id = "sta_div",
      fluidPage(
        id = "pos_page",
        tags$p("Choose a stationary period", style = "font-weight:bold;"),
        fluidRow(
          column(2, style = "padding:0px;", actionButton("prev_pos", "<", width = "100%")),
          column(8, style = "padding:0px;", selectInput("i_sta", label = NULL, choices = "1")),
          column(2, style = "padding:0px;", actionButton("next_pos", ">", width = "100%")),
        )
      ),

      fluidRow(
        column(6, htmlOutput("fl_prev_info")),
        column(6, htmlOutput("fl_next_info")),
      ),
      tags$hr(),
      tags$p("Explitory text"),
      fluidPage(
        id = "thr_sta_page",
        fluidRow(
          column(6, actionButton("edit_pos", "Edit the position")),
          column(6, actionButton("query_pos", "Query pressure")),
        )
      ),
      tags$hr(),
      sliderInput("speed", "Groundspeed limit [km/h]", min = 0, max = 200, value = 40, step = 10),
      checkboxGroupInput("map_source",
        label = "Probability map to display",
        choices = c("Pressure", "Light"),
        selected = c("Pressure", "Light"), inline = T
      ),
    )
  ),
  fixedPanel(
    bottom = 0, left = 0, width = "100%", height = "300px",
    plotlyOutput("pressure_graph")
  ),
)

server <- function(input, output, session) {

  # Read input static_prob
  load("../geopressureviz.RData")

  # Hide selector if light doesn't exist
  if (!exists("light_prob")) {
    shinyjs::hidden(map_source)
  }

  # Get flight information
  sta <- do.call("rbind", lapply(static_prob, function(r) {
    mt <- metadata(r)
    mt$start <- mt$temporal_extent[1]
    mt$end <- mt$temporal_extent[2]
    mt$duration <- as.numeric(difftime(mt$end, mt$start, units = "days"))
    mt <- within(mt, rm(flight, temporal_extent, max_sample, margin))
    as.data.frame(mt)
  }))

  col <- rep(RColorBrewer::brewer.pal(12, "Set3"), times = 10)
  sta$col <- col[sta$sta_id]

  flight <- lapply(static_prob, function(r) {
    fl <- metadata(r)$flight
    if (length(fl) > 0) {
      fl$duration <- mapply(function(s, e) {
        as.numeric(difftime(e, s, units = "hours"))
      }, fl$start, fl$end)
    } else {
      fl$duration <- 0
    }
    fl
  })

  reactVal <- reactiveValues()
  reactVal$path <- do.call("rbind", lapply(static_prob, function(r) {
    idx <- which.max(r)
    pos <- xyFromCell(r, idx)
    data.frame(
      lon = pos[1],
      lat = pos[2]
    )
  }))

  if (exists("pressure_timeserie")) {
    reactVal$ts <- pressure_timeserie[sta$sta_id]
  } else {
    reactVal$ts <- list()
  }

  observe({
    id <- sta$duration >= as.numeric(input$thr_sta)
    tmp <- as.list(sta$sta_id[id])
    names(tmp) <- paste0('#',sta$sta_id[id]," (" , round(sta$duration[id],1), "d.)")
    updateSelectizeInput(session, "i_sta", choices = tmp)
  }) %>% bindEvent(input$thr_sta)

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
    i <- min(max(i + 1, 1), length(tmp) - 1)
    updateSelectizeInput(session, "i_sta", selected = tmp[i])
  })

  observeEvent(input$allsta, {
    if (input$allsta) {
      shinyjs::hide(id = "sta_div")
      shinyjs::show(id = "thr_sta_page")
    } else {
      shinyjs::show(id = "sta_div")
      shinyjs::hide(id = "thr_sta_page")
    }
  })


  nsta <- length(static_prob)

  flight_duration <- reactive({
    id <- which(sta$duration >= as.numeric(input$thr_sta))
    flight_duration <- c()
    for (i_f in seq_len(length(id) - 1)) {
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

  output$fl_prev_info <- renderUI({
    req(input$i_sta)
    fl_dur <- flight_duration()
    id <- which(sta$duration >= as.numeric(input$thr_sta))
    sta_id_thr <- sta$sta_id[id]
    i_s_thr <- which(as.numeric(input$i_sta) == sta_id_thr)
    if (i_s_thr!=1){
      dist <- geosphere::distGeo(reactVal$path[id[i_s_thr-1],], reactVal$path[id[i_s_thr],])/1000
      HTML(
        sta_id_thr[i_s_thr]-sta_id_thr[i_s_thr-1]," flights -",
        round(fl_dur[i_s_thr - 1])," hrs<br>",
        round(dist)," km - ",
        round(dist/fl_dur[i_s_thr - 1]), "km/h"
      )
    } else {
      HTML("")
    }
  })

  output$fl_next_info <- renderUI({
    req(input$i_sta)
    fl_dur <- flight_duration()
    id <- which(sta$duration >= as.numeric(input$thr_sta))
    sta_id_thr <- sta$sta_id[id]
    i_s_thr <- which(as.numeric(input$i_sta) == sta_id_thr)
    if (i_s_thr!= length(sta_id_thr)){
      dist <- geosphere::distGeo(reactVal$path[id[i_s_thr+1],], reactVal$path[id[i_s_thr],])/1000

      HTML(
        sta_id_thr[i_s_thr+1]-sta_id_thr[i_s_thr]," flights -",
        round(sum(fl_dur[i_s_thr]))," hrs<br>",
        round(dist)," km - ",
        round(dist/fl_dur[i_s_thr]), "km/h"
      )
    } else {
      HTML("")
    }
  })

  output$map <- renderLeaflet({
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels,
        options = providerTileOptions(noWrap = TRUE)
      )
  })

  reactVal$isEdit <- F
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


  observeEvent(input$edit_pos, {
    if (reactVal$isEdit) {
      reactVal$isEdit <- F
      updateActionButton(session, "edit_pos", label = "Edit position")
      removeClass("edit_pos", "primary")
    } else {
      reactVal$isEdit <- T
      updateActionButton(session, "edit_pos", label = "Stop editing")
      addClass("edit_pos", "primary")
    }
  })

  observeEvent(input$query_pos, {
    print("test")
    if (reactVal$isEdit) {
      return()
    }
    print("test2")
    id <- which(as.numeric(input$i_sta) == sta$sta_id)
    pam_pressure_sta <- subset(pam_data$pressure, sta_id == input$i_sta)
    print("sens")
    reactVal$ts[[id]] <- geopressure_ts(reactVal$path$lon[id], reactVal$path$lat[id],
      pressure = pam_pressure_sta
    )
    reactVal$ts[[id]]["sta_id"] <- input$i_sta
    reactVal$ts[[id]]$pressure0 <- reactVal$ts[[id]]$pressure - mean(reactVal$ts[[id]]$pressure) + mean(pam_pressure_sta$obs[!pam_pressure_sta$isoutliar])
    updateSelectizeInput(session, "i_sta", selected = input$i_sta)
    print("finished")
  })



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
  })


  observe({
    proxy <- leafletProxy("map") %>%
      clearShapes() %>%
      clearImages()
    req(input$i_sta)
    id <- which(sta$duration >= as.numeric(input$thr_sta))
    sta_thr <- sta[id, ]
    path_thr <- reactVal$path[id, ]
    fl_dur <- flight_duration()

    if (input$allsta) {
      proxy <- proxy %>%
        addPolylines(lng = path_thr$lon, lat = path_thr$lat, opacity = 1, color = "#FFF", weight = 3) %>%
        addCircles(
          lng = path_thr$lon, lat = path_thr$lat, opacity = 1, weight = sta_thr$duration^(0.3) * 10,
          label = paste0("#", sta_thr$sta_id, ", ", round(sta_thr$duration, 1), " days"), color = sta_thr$col
        )
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
      if (i_s != nsta) {
        proxy <- proxy %>%
          addCircles(lng = path_thr$lon[i_s_thr + 1], lat = path_thr$lat[i_s_thr + 1], opacity = 1, color = sta_thr$col[i_s_thr + 1], weight = sta_thr$duration[i_s_thr]^(0.3) * 10) %>%
          addCircles(lng = path_thr$lon[i_s_thr + 1], lat = path_thr$lat[i_s_thr + 1], opacity = 1, color = sta_thr$col[i_s_thr + 1], radius = as.numeric(input$speed) * sum(fl_dur[i_s_thr]) * 1000, fillOpacity = 0, weight = 2)
      }
    }
    proxy
  }) # %>% bindEvent(input$i_sta)


  output$pressure_graph <- renderPlotly({
    req(input$thr_sta)
    id <- which(sta$duration >= as.numeric(input$thr_sta))
    p <- ggplot() +
      geom_line(data = pam_data$pressure, aes(x = date, y = obs), colour = "grey") +
      geom_point(data = subset(pam_data$pressure, isoutliar), aes(x = date, y = obs), colour = "black") +
      theme_bw()

    for (i in id) {
      p <- p +
        geom_line(data = reactVal$ts[[i]], aes(x = date, y = pressure0), col = sta$col[i])
    }

    ggplotly(p, dynamicTicks = T, height = 300) %>%
      layout(
        showlegend = F,
        yaxis = list(title = "Pressure [hPa]", autorange = F),
        xaxis = list(autorange = F)
      )
  })

  observe({
    if (!input$allsta) {
      i_s <- as.numeric(input$i_sta)
      i_sta <- i_s == sta$sta_id
      id <- pam_data$pressure$sta_id == i_s
      plotlyProxy("pressure_graph", session) %>%
        plotlyProxyInvoke(
          "relayout",
          list(
            yaxis = list(range = c(min(pam_data$pressure$obs[id]) - 5, max(pam_data$pressure$obs[id]) + 5)),
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

  # leafletProxy("map", data = crime) %>%
  #   setView(long, lat, zoom = 14) %>%
  #   clearShapes() %>%
  #   clearControls() %>%
  #   addCircles(~long, ~lat, stroke = FALSE, fill = TRUE, fillOpacity = .7,
  #              color = ~leafPal(top5), label = ~category, radius = 30) %>%
  #   addLegend("bottomright", pal = leafPal, values = ~top5, title = "Category")
}

shinyApp(ui, server)
