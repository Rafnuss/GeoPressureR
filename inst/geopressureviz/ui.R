ui <- bootstrapPage(
  useShinyjs(),
  shiny::tags$head(
    shiny::tags$link(rel = "shortcut icon",
              href = "https://raphaelnussbaumer.com/GeoPressureR/favicon-16x16.png"),
    shiny::tags$link(href = "https://fonts.googleapis.com/css?family=Oswald", rel = "stylesheet"),
    shiny::tags$style(type = "text/css", "html, body {width:100%;height:100%; font-family: Oswald,
               sans-serif;}.primary{background-color:#007bff; color: #fff;}.js-plotly-plot
               .plotly .modebar{left: 0}"),
    # includeHTML("meta.html"),
  ),
  leaflet::leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(
    top = 0, left = 0, draggable = FALSE, width = "200px",
    style = "z-index:500; min-width: 300px;padding-left: 50px",
    shiny::tags$h2("GeoPressureViz", style = "color:white;"),
    shiny::tags$a("About GeoPressureR", href = "https://raphaelnussbaumer.com/GeoPressureR/",
           style = "display: block;padding-bottom:20px;"),
  ),
  absolutePanel(
    top = 0, right = 0, draggable = FALSE, width = "200px",
    style = "z-index:500; min-width: 300px;padding: 5px 10px;background-color:white;",
    fluidPage(
      fluidRow(
        column(3, htmlOutput("tag_id")),
        column(9, div(
          style = "text-align: center;",
          "Full Track",
          switchInput("full_track", value = TRUE, inline = TRUE, size = "small")
        ))
      )
    ),
    fluidPage(
      id = "track_info_view",
      fluidRow(
        column(8, shiny::tags$p("Minimum duration [days]",
                         style = "font-weight:bold; line-height: 34px;text-align: right;")),
        column(4, style = "padding:0px;", numericInput("min_dur_stap", NULL, min = 0, max = 50,
                                                       value = 0, step = 0.5))
      ),
      fluidRow(
        actionButton("export_path", "Export path to interim",
        style = "background-color: #28a745; color: white; width: 100%;")
      )
    ),
    div(
      id = "stap_info_view",
      fluidPage(
        shiny::tags$p("Choose a stationary period", style = "font-weight:bold;"),
        fluidRow(
          column(2, style = "padding:0px;", actionButton("previous_position", "<", width = "100%")),
          column(8, style = "padding:0px;", selectInput("stap_id", label = NULL, choices = "1")),
          column(2, style = "padding:0px;", actionButton("next_position", ">", width = "100%"))
        )
      ),
      fluidRow(
        column(6, htmlOutput("flight_prev_info")),
        column(6, htmlOutput("flight_next_info"))
      ),
      shiny::tags$hr(),
      sliderInput("speed", "Groundspeed limit [km/h]", min = 0, max = 150, value = 40, step = 10),
      div(
        radioButtons("map_source",
          label = "Probability map to display", inline = TRUE, choices = names(.maps),
          selected = tail(names(.maps), 1)
        ),
        shiny::tags$hr(),
        shiny::tags$p("Change position by clicking on the map and update the pressure time series."),
        fluidPage(
          id = "edit_query_position_id",
          fluidRow(
            column(6, actionButton("edit_position", "Edit position")),
            column(6, actionButton("query_position", "Query pressure"))
          )
        ),
        checkboxInput("edit_position_interpolate",
                      label = "Interpolate positions between stap", value = FALSE)
      )
    )
  ),
  fixedPanel(
    bottom = 0, left = 0, width = "100%", height = "300px",
    plotly::plotlyOutput("pressure_plot")
  )
)