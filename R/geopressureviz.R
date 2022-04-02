library(plotly)
library(shinyjs)
library(GeoPressureR)
library(shiny)
library(leaflet)
library(plotly)
library(leaflet.extras)
library(raster)

ui <- bootstrapPage(

  tags$head(
   tags$link(href = "https://fonts.googleapis.com/css?family=Oswald", rel = "stylesheet"),
   tags$style(type = "text/css", "html, body {width:100%;height:100%; font-family: Oswald, sans-serif;}"),
  #   # includeHTML("meta.html"),
  #   tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
  #               type="text/javascript"),
  #   tags$script('
  #               $(document).ready(function () {
  #                 navigator.geolocation.getCurrentPosition(onSuccess, onError);
  #
  #                 function onError (err) {
  #                   Shiny.onInputChange("geolocation", false);
  #                 }
  #
  #                 function onSuccess (position) {
  #                   setTimeout(function () {
  #                     var coords = position.coords;
  #                     console.log(coords.latitude + ", " + coords.longitude);
  #                     Shiny.onInputChange("geolocation", true);
  #                     Shiny.onInputChange("lat", coords.latitude);
  #                     Shiny.onInputChange("long", coords.longitude);
  #                   }, 1100)
  #                 }
  #               });
  #               ')
   ),

  leafletOutput("map", width = "100%", height = "100%"),

  fixedPanel(
    bottom = 0, left = 0, width = "100%", height = "300px",
    plotlyOutput("pressure_graph")
  ),

  absolutePanel(
    top = 10, right = 10, style = "z-index:500; text-align: right;",
    tags$h2("GeoPressureViz"),
    tags$a("About GeoPressureR", href="https://raphaelnussbaumer.com/GeoPressureR/")
  ),

  absolutePanel(
    top = 100, left = 10, draggable = TRUE, width = "20%", style = "z-index:500; min-width: 300px;",
    selectInput("map_source", "Choose the map to display: ", choices=c("combined","pressure","light"), selected = "combined"),
    selectInput("i_sta", "Choose a stationary period", choices="", selected="all"),
    numericInput("speed", "Define the speed limit [km/h]", min = 0, max = 1000, value=80, step=10),
    numericInput("thr_sta", "Sta to consider [days]", min = 0, max = 50, value=3, step=0.5),
    textOutput("sta_info"),
    actionButton("edit_pos","Edit the position")
  )
)

server <- function(input, output, session) {

  # Read input static_prob
  load("../geopressureviz.RData")

  # Hide selector if light doesn't exist
  if (!exists("light_prob")){
    # shinyjs::hidden(map_source)
  }

  # Get flight information
  sta <- do.call("rbind",lapply(static_prob, function(r) {
    mt <- metadata(r)
    mt$start <- mt$temporal_extent[1]
    mt$end <- mt$temporal_extent[2]
    mt$duration <- as.numeric(difftime(mt$end,mt$start,units="days"))
    mt <- within(mt, rm(flight,temporal_extent, max_sample, margin))
    as.data.frame(mt)
  }))

  col <- rep(RColorBrewer::brewer.pal(12, "Set3"), times = 10)
  sta$col <- col[sta$sta_id]

  flight <- lapply(static_prob, function(r) {
    fl <- metadata(r)$flight
    if (length(fl)>0){
      fl$duration <- mapply(function(s,e){
        as.numeric(difftime(e,s,units="hours"))
      },fl$start, fl$end)
    } else {
      fl$duration = 0
    }
    fl
  })

  reactVal <- reactiveValues()
  reactVal$path = do.call("rbind",lapply(static_prob, function(r) {
    idx = which.max(r)
    pos <- xyFromCell(r,idx)
    data.frame(
      lon = pos[1],
      lat = pos[2]
    )
  }))

  observe({
    id <- sta$duration >= as.numeric(input$thr_sta)
    updateSelectizeInput(session, "i_sta", choices = c("all", sta$sta_id[id]))
  }) %>% bindEvent(input$thr_sta)


  nsta <- length(static_prob)

  flight_duration <- reactive({
    id <- which(sta$duration >= as.numeric(input$thr_sta))
    flight_duration = c()
    for (i_f in seq_len(length(id)-1)){
      from_sta_id <- id[i_f]
      to_sta_id <- id[i_f+1]

      flight_duration[i_f] <- sum(
        do.call(rbind,lapply(
          flight[seq(from_sta_id, to_sta_id)],
          function(x){
        sum(x$duration)
      })))
    }
      flight_duration
  }) %>% bindEvent(input$thr_sta)



  output$sta_info <- renderText({
    req(input$i_sta)
    if (input$i_sta != "all"){
      i_sta <- which(as.numeric(input$i_sta)==sta$sta_id)
      txt <- paste("Stationary period ", sta$sta_id[i_sta] ,"from ",
             format(sta$start[i_sta],"%d %B %H:%M"), " to ", format(sta$end[i_sta],"%d %B %H:%M"),
            "( ", round(sta$duration[i_sta]) ,"days )\n")
    } else {
      ""
    }
  })

  # as.data.frame(flight[[i_sta]])


  output$map <- renderLeaflet({
      map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels,
                       options = providerTileOptions(noWrap = TRUE)
      )
  })

  reactVal$isEdit <- F
  observeEvent(input$map_click,{
    if(is.null(click) ){
      return()
    }
    click = input$map_click
    if( !reactVal$isEdit ){
      return()
    }
    if (input$i_sta!="all"){
      i_sta <- which(as.numeric(input$i_sta)==sta$sta_id)
      reactVal$path[i_sta,] <- c(click$lng, click$lat)
      print(click)
    }
  })


  observeEvent(input$edit_pos,{
    if (reactVal$isEdit){
      reactVal$isEdit=F
      updateActionButton(session,"edit_pos",label="Edit position")
    } else {
      reactVal$isEdit=T
      updateActionButton(session,"edit_pos",label="Stop editing")
    }
  })


  map_prob <- reactive({
    if (input$map_source=="combined"){
      static_prob
    }else if (input$map_source=="pressure"){
      pressure_prob
    }else if (input$map_source=="light"){
      light_prob
    }
  })


  observe({
  proxy <- leafletProxy("map") %>%
      clearShapes() %>% clearImages()
      req(input$i_sta)
      id <- which(sta$duration >= as.numeric(input$thr_sta))
      sta_thr <- sta[id,]
      path_thr <- reactVal$path[id,]
      fl_dur <- flight_duration()

      if( input$i_sta=="all") {
        proxy <- proxy %>%
          addPolylines(lng = path_thr$lon, lat = path_thr$lat, opacity = 1, color = "#808080", weight = 3) %>%
          addCircles(lng = path_thr$lon, lat = path_thr$lat, opacity = 1, weight = sta$duration^(0.3)*10, label= paste0("i_sta=",sta_thr$sta_id,", ",round(sta_thr$duration)," days"), color = sta_thr$col)
      } else {
        i_sta <- which(as.numeric(input$i_sta)==sta_thr$sta_id)
        tmp <- map_prob()
        proxy <- proxy %>%
          addRasterImage(tmp[[i_sta]], opacity = 0.8, colors = "viridis") %>%
          addPolylines(lng = path_thr$lon, lat = path_thr$lat, opacity = .1, color = "#808080", weight = 3) %>%
          addPolylines(lng = path_thr$lon[i_sta+(-1:1)], lat = path_thr$lat[i_sta+(-1:1)], opacity = 1, color = "#808080", weight = 3) %>%
          addCircles(lng = path_thr$lon, lat = path_thr$lat, opacity = .1, color = "#000", weight = sta_thr$duration^(0.3)*10) %>%
          addCircles(lng = path_thr$lon[i_sta], lat = path_thr$lat[i_sta], opacity = 1, weight = sta_thr$duration[i_sta]^(0.3)*10, color = sta_thr$col[i_sta])

        if (i_sta!=1){
          proxy <- proxy %>%
            addCircles(lng = path_thr$lon[i_sta-1], lat = path_thr$lat[i_sta-1], opacity = 1, color = "green", weight = sta_thr$duration[i_sta-1]^(0.3)*10) %>%
            addCircles(lng = path_thr$lon[i_sta-1], lat = path_thr$lat[i_sta-1], opacity = 1, color = "green", radius = as.numeric(input$speed)*sum(fl_dur[i_sta-1])*1000, fillOpacity=0, weight = 2)
        }
        if (i_sta!=nsta){
          proxy <- proxy %>%
            addCircles(lng = path_thr$lon[i_sta+1], lat = path_thr$lat[i_sta+1], opacity = 1, color = "blue", weight = sta_thr$duration[i_sta]^(0.3)*10) %>%
            addCircles(lng = path_thr$lon[i_sta+1], lat = path_thr$lat[i_sta+1], opacity = 1, color = "blue", radius = as.numeric(input$speed)*sum(fl_dur[i_sta])*1000, fillOpacity=0, weight = 2)
}
      }
  proxy
  }) # %>% bindEvent(input$i_sta)


  output$pressure_graph <- renderPlotly({

    req(input$thr_sta)
    id <- which(sta$duration >= as.numeric(input$thr_sta))
    staid <- sta$sta_id[id]

    p <- ggplot() +
      geom_line(data = pam_data$pressure, aes(x = date, y = obs), colour = "grey") +
      geom_point(data = subset(pam_data$pressure, isoutliar), aes(x = date, y = obs), colour = "black") +
      geom_line(data = do.call("rbind", pressure_timeserie[staid]), aes(x = date, y = pressure0, col = as.factor(sta_id))) +
      theme_bw() +
      scale_colour_manual(values = col)

    ggplotly(p, dynamicTicks = T, height = 300) %>%
      layout(
        showlegend = F,
        yaxis = list(title = "Pressure [hPa]", autorange = F),
        xaxis = list(autorange = F)
      )
  })

  observe({
    req(input$i_sta)
    if (input$i_sta!="all"){
      i_s <- as.numeric(input$i_sta)
      i_sta <- i_s==sta$sta_id
      id <- pam_data$pressure$sta_id==i_s
      plotlyProxy("pressure_graph", session) %>%
        plotlyProxyInvoke(
          "relayout",
          list(yaxis = list(range = c(min(pam_data$pressure$obs[id])-5, max(pam_data$pressure$obs[id])+5)),
               xaxis = list(range = c(sta$start[i_sta]-60*60*24, sta$end[i_sta]+60*60*24)))
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
