library(shiny)
library(plotly)
library(bslib)

ui <- fluidPage(
  shinyjs::useShinyjs(), # Enable shinyjs for UI control
  title = "Trainset", # This will be updated dynamically by server
  class = "px-0", # Bootstrap class to remove left and right padding
  theme = bslib::bs_theme(
    bootswatch = "flatly",
    base_font = bslib::font_google("Inter")
  ),
  shiny::tags$head(
    shiny::tags$link(
      rel = "shortcut icon",
      href = "https://raphaelnussbaumer.com/GeoPressureR/favicon-16x16.png"
    ),
    shiny::tags$script(src = "plot_controls.js"),
    shiny::tags$script(src = "plotly_events.js"),
    shiny::tags$script(HTML(
      "
      Shiny.addCustomMessageHandler('updateTitle', function(title) {
        document.title = title;
      });
    "
    ))
  ),

  # Custom header with Bootstrap styling
  div(
    class = "d-flex justify-content-between align-items-center p-3 bg-primary text-white mb-0",
    div(class = "h3 mb-0", "GeoPressure Trainset"),
    div(
      class = "d-flex gap-3 align-items-center",
      # Stap ID selector - only show if stap data exists
      conditionalPanel(
        condition = "output.stap_data_available == true",
        div(
          class = "d-flex flex-column",
          shiny::tags$label(class = "form-label text-white small mb-1", "Stap ID:"),
          div(
            class = "input-group align-items-center",
            actionButton("stap_id_prev", "<", class = "form-group btn btn-outline-light"),
            selectInput(
              "stap_id",
              NULL,
              choices = c("None" = ""), # Will be updated by server
              selected = "",
              width = "120px"
            ),
            actionButton("stap_id_next", ">", class = "form-group btn btn-outline-light"),
            shinyjs::hidden(
              actionButton(
                "compute_stap_btn",
                NULL,
                icon = icon("arrow-rotate-right"),
                class = "form-group btn btn-warning"
              )
            )
          )
        )
      ),

      # Active Series selector - only show if acceleration data exists
      conditionalPanel(
        condition = "output.acceleration_data_available == true",
        div(
          class = "d-flex flex-column",
          shiny::tags$label(class = "form-label text-white small mb-1", "Active Series:"),
          selectInput(
            "active_series",
            NULL,
            choices = c("Pressure" = "pressure", "Acceleration" = "acceleration"),
            selected = "pressure",
            width = "140px"
          )
        )
      ),

      # Label selector with add button
      div(
        class = "d-flex flex-column",
        shiny::tags$label(class = "form-label text-white small mb-1", "Label:"),
        div(
          class = "input-group",
          selectInput(
            "label_select",
            NULL,
            choices = c("flight", "discard"), # Initial choices, will be updated by server
            selected = "flight",
            width = "120px"
          ),
          actionButton("add_label_btn", "+", class = "form-group btn btn-outline-light")
        )
      ),

      # Export button
      div(
        class = "d-flex flex-column justify-content-end",
        downloadButton("export_btn", "Export", class = "btn btn-success", icon = icon("download"))
      )
    )
  ),

  # Plot area with Bootstrap styling
  div(
    class = "position-relative",
    style = "height: calc(100vh - 111px);",
    plotlyOutput("ts_plot", width = "100%", height = "100%")
  )
)
