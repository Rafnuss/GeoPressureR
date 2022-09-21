
ui <- bootstrapPage(
  useShinyjs(),
  tags$head(
    tags$link(rel="shortcut icon", href="https://raphaelnussbaumer.com/GeoPressureR/favicon-16x16.png"),
    tags$link(href = "https://fonts.googleapis.com/css?family=Oswald", rel = "stylesheet"),
    tags$style(type = "text/css", "html, body {width:100%;height:100%; font-family: Oswald, sans-serif;}.primary{background-color:#007bff; color: #fff;}.js-plotly-plot .plotly .modebar{left: 0}"),
    # includeHTML("meta.html"),
  ),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(
    top = 0, left = 0, draggable = F, width = "200px", style = "z-index:500; min-width: 300px;padding-left: 50px",
    tags$h2("GeoPressureViz", style = "color:white;"),
    tags$a("About GeoPressureR", href = "https://raphaelnussbaumer.com/GeoPressureR/", style = "display: block;padding-bottom:20px;"),
  ),
  absolutePanel(
    top = 0, right = 0, draggable = F, width = "200px", style = "z-index:500; min-width: 300px;padding: 5px 10px;background-color:white;",
    fluidPage(
      fluidRow(
        column(3, htmlOutput("gdl_id")),
        column(9, div(
          style = "text-align: center;",
          "Full Track",
          switchInput("allsta", value = TRUE, inline = T, size = "small")
        ))
      )
    ),
    fluidPage(
      id = "thr_sta_page",
      fluidRow(
        column(8, tags$p("Minimum duration [days]", style = "font-weight:bold; line-height: 34px;text-align: right;"), ),
        column(4, style = "padding:0px;", numericInput("thr_sta", NULL, min = 0, max = 50, value = 0, step = 0.5), ),
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
      sliderInput("speed", "Groundspeed limit [km/h]", min = 0, max = 200, value = 40, step = 10),
      div(
        id = "map_source_div",
        radioButtons("map_source",
          label = "Probability map to display", inline = T, choices = .map_choices, selected = map_choices[tail(which(!is.na(.map_val)), 1)]
        ),
        tags$hr(),
        tags$p("Change position by clicking on the map and update the pressure timeserie."),
        fluidPage(
          id = "thr_sta_page",
          fluidRow(
            column(6, actionButton("edit_pos", "Start editing")),
            column(6, actionButton("query_pos", "Query pressure")),
          )
        )
      )
    )
  ),
  fixedPanel(
    bottom = 0, left = 0, width = "100%", height = "300px",
    plotlyOutput("pressure_graph")
  ),
)
