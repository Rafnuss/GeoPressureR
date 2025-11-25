# Handle compute_stap_btn click: update pressure labels and recompute stap
observeEvent(input$compute_stap_btn, {
  tag$pressure$label <- reactive_label_pres()
  tag$acceleration$label <- reactive_label_acc()
  tag <<- tag_label_stap(tag, quiet = TRUE)

  # Update reactive labels with new stap labels
  reactive_label_pres(tag$pressure$label)
  reactive_label_acc(tag$acceleration$label)
  stap_data <<- if (!is.null(tag$stap)) {
    tag$stap
  } else {
    data.frame(
      start = as.POSIXct(character(0)),
      end = as.POSIXct(character(0)),
      stap_id = character(0)
    )
  }
  showNotification("STAP recomputed.", type = "message", duration = 2)
})
library(shiny)
library(plotly)
library(shinyjs)
library(GeoPressureR)


# Source utility functions
source("utils.R")

tag <- shiny::getShinyOption("tag")

# Check if tag is provided
if (is.null(tag)) {
  cli::cli_abort(
    "No tag data found in shiny options. Please provide a valid tag object with {.fun trainset} or {.code shiny::shinyOptions(tag = tag)}."
  )
}

# Preprocess tag data before server starts
# Extract data once for efficiency
pressure_data <- tag$pressure
# Add empty label for NA labels if not present
if (!"label" %in% names(pressure_data)) {
  pressure_data$label <- ""
}

has_acceleration <- !is.null(tag$acceleration) && nrow(tag$acceleration) > 0

acceleration_data <- if (has_acceleration) {
  tag$acceleration
} else {
  data.frame(date = as.POSIXct(character(0)), value = numeric(0), label = character(0))
}
if (!"label" %in% names(acceleration_data)) {
  acceleration_data$label <- ""
}

has_stap <- !is.null(tag$stap) && nrow(tag$stap) > 0
stap_data <- if (has_stap) {
  tag$stap
} else {
  data.frame(
    start = as.POSIXct(character(0)),
    end = as.POSIXct(character(0)),
    stap_id = character(0)
  )
}

stap_data$duration <- stap2duration(stap_data)

server <- function(input, output, session) {
  # Disable add_label_btn when acceleration is active
  observe({
    if (!is.null(input$active_series) && input$active_series == "acceleration") {
      shinyjs::disable("add_label_btn")
    } else {
      shinyjs::enable("add_label_btn")
    }
  })
  # Update browser tab title with tag ID
  updateTabsetPanel(session, inputId = NULL)
  session$sendCustomMessage("updateTitle", glue::glue("Trainset - {tag$param$id}"))

  # Handle session end - stop app when browser is closed
  session$onSessionEnded(function() {
    stopApp()
  })

  # Provide availability flags for conditional UI
  output$acceleration_data_available <- reactive({
    has_acceleration
  })
  outputOptions(output, "acceleration_data_available", suspendWhenHidden = FALSE)

  output$stap_data_available <- reactive({
    has_stap
  })
  outputOptions(output, "stap_data_available", suspendWhenHidden = FALSE)

  # Initialize reactive data with preprocessed data
  reactive_label_pres <- reactiveVal(pressure_data$label)
  reactive_label_acc <- reactiveVal(acceleration_data$label)

  #----- LABELS -----
  # Initialize stap_elev_count from highest elev_xxx in pressure_data$label
  elev_labels <- grep("^elev_\\d+$", pressure_data$label, value = TRUE)
  elev_nums <- as.integer(sub("elev_", "", elev_labels))
  stap_elev_count <- reactiveVal(
    if (length(elev_nums) > 0 && !all(is.na(elev_nums))) max(elev_nums, na.rm = TRUE) else 1
  )

  # Handle add label button click
  observeEvent(input$add_label_btn, {
    stap_elev_count(stap_elev_count() + 1)
  })

  # Dynamically update label choices based on active_series and stap_elev_count
  observe({
    series <- input$active_series
    if (series == "acceleration") {
      label_choices <- "flight"
    } else {
      label_choices <- c("discard")
      if (!has_acceleration) {
        label_choices <- c(label_choices, "flight")
      }
      label_choices <- c(label_choices, paste0("elev_", seq(1, stap_elev_count())))
    }
    session$onFlushed(function() {
      updateSelectInput(
        session,
        "label_select",
        choices = label_choices,
        selected = label_choices[[length(label_choices)]]
      )
    })
    # Show or hide compute_stap_btn based on presence of 'flight' label
    if ("flight" %in% label_choices) {
      shinyjs::show("compute_stap_btn")
    } else {
      shinyjs::hide("compute_stap_btn")
    }
  })

  #----- STAP -----
  # Update stap_id choices only if stap_data exists
  if (has_stap) {
    # Stap data available - show selector and populate choices
    stap_choices <- c(
      "None" = "",
      setNames(
        stap_data$stap_id,
        glue::glue("#{stap_data$stap_id} ({round(stap_data$duration, 1)}d)")
      )
    )

    session$onFlushed(
      function() {
        updateSelectInput(
          session,
          "stap_id",
          choices = stap_choices,
          selected = ""
        )
      },
      once = TRUE
    )

    observeEvent(input$stap_id_prev, {
      current_stap <- input$stap_id

      if (current_stap == "" || is.null(current_stap)) {
        # If no stap selected, select the last one
        new_stap <- stap_data$stap_id[1]
      } else {
        # Find current index and move to previous
        current_index <- which(stap_data$stap_id == current_stap)
        if (length(current_index) > 0 && current_index > 1) {
          new_stap <- stap_data$stap_id[current_index - 1]
        } else {
          # If at first stap, go to "None"
          new_stap <- ""
        }
      }
      updateSelectInput(session, "stap_id", selected = new_stap)
    })

    observeEvent(input$stap_id_next, {
      current_stap <- input$stap_id

      if (current_stap == "" || is.null(current_stap)) {
        # If no stap selected, select the first one
        new_stap <- stap_data$stap_id[1]
      } else {
        # Find current index and move to next
        current_index <- which(stap_data$stap_id == current_stap)
        if (length(current_index) > 0 && current_index < length(stap_data$stap_id)) {
          new_stap <- stap_data$stap_id[current_index + 1]
        } else {
          # If at last stap, stay there (or could cycle back to first)
          new_stap <- current_stap
        }
      }
      updateSelectInput(session, "stap_id", selected = new_stap)
    })
  }

  # Handle stap_id selection to set x-axis limits only if stap data exists
  observeEvent(input$stap_id, {
    if (!is.null(input$stap_id) && input$stap_id != "") {
      # Find the selected stap
      selected_stap <- stap_data[stap_data$stap_id == input$stap_id, ]

      if (nrow(selected_stap) > 0) {
        lag_x <- 60 * 60 * 24 / 2 # 1 day in seconds
        lag_y <- 5 # Pressure units

        pressure_val_stap_id <- pressure_data$value[
          pressure_data$date >= selected_stap$start & pressure_data$date <= selected_stap$end
        ]

        # Update the plot x-axis range
        layout_update <- list(
          xaxis = list(range = list(selected_stap$start - lag_x, selected_stap$end + lag_x)),
          yaxis = list(
            range = c(min(pressure_val_stap_id) - lag_y, max(pressure_val_stap_id) + lag_y)
          )
        )

        plotlyProxy("ts_plot", session) |>
          plotlyProxyInvoke("relayout", layout_update)
      }
    }
  })

  # Update active_series choices based on available data
  init_active_series <- if (has_stap) "pressure" else "pressure" # Default to pressure if no acceleration
  if (has_acceleration) {
    # Both pressure and acceleration available - selector will be shown by conditionalPanel
    updateSelectInput(
      session,
      "active_series",
      choices = c("Pressure" = "pressure", "Acceleration" = "acceleration"),
      selected = init_active_series
    )
  }

  # Get initial styling
  initial_styles <- apply_plot_styling(
    NULL, # No proxy for initial creation
    init_active_series,
    pressure_data$label, # Use initial labels, not reactive values
    acceleration_data$label, # Use initial labels, not reactive values
    NULL # No session for initial creation
  )

  # Pre-compute static values that don't change
  time_range <- list(
    min(c(pressure_data$date, acceleration_data$date)),
    max(c(pressure_data$date, acceleration_data$date))
  )

  # Main plot rendering - static plot created only once
  output$ts_plot <- renderPlotly({
    # Create the plot with dual y-axes (optimized for performance)
    p <- plot_ly() |>
      # Add pressure LINE trace for range slider visibility
      add_trace(
        data = pressure_data,
        x = ~date,
        y = ~value,
        type = "scatter", # Regular scatter for range slider compatibility
        mode = "lines", # Lines only for range slider
        name = "Pressure_line",
        line = list(
          width = initial_styles$pressure_line_style$line.width,
          color = initial_styles$pressure_line_style$line.color
        ),
        yaxis = "y", # Use primary y-axis for range slider compatibility
        hovertemplate = "Pressure: %{y}<br>Time: %{x}<extra></extra>",
        showlegend = FALSE,
        visible = TRUE
      ) |>
      # Add pressure MARKERS trace for interaction and performance
      add_trace(
        data = pressure_data,
        x = ~date,
        y = ~value,
        type = "scattergl", # Use WebGL for better performance with many points
        mode = "markers", # Markers only for interaction
        name = "Pressure",
        marker = list(
          size = initial_styles$pressure_markers_style$marker.size,
          color = initial_styles$pressure_markers_style$marker.color[[1]], # Extract from list
          opacity = initial_styles$pressure_markers_style$marker.opacity
        ),
        yaxis = "y", # Same y-axis as line trace
        hoverinfo = "skip", # Make sure hover is enabled
        showlegend = FALSE
      )

    # Conditionally add acceleration trace if acceleration data exists
    if (has_acceleration) {
      p <- p |>
        add_trace(
          data = acceleration_data,
          x = ~date,
          y = ~value,
          type = "scattergl", # Use WebGL for better performance
          mode = "lines+markers", # Drop lines for large datasets
          name = "Acceleration",
          line = list(
            width = initial_styles$acceleration_style$line.width,
            color = initial_styles$acceleration_style$line.color
          ),
          marker = list(
            size = initial_styles$acceleration_style$marker.size,
            color = initial_styles$acceleration_style$marker.color[[1]], # Extract from list
            opacity = initial_styles$acceleration_style$marker.opacity
          ),
          yaxis = "y2",
          hovertemplate = "Acceleration: %{y}<br>Time: %{x}<extra></extra>", # Simplified hover
          showlegend = FALSE,
          visible = TRUE
        )
    }

    # Create layout configuration conditionally
    layout_config <- list(
      xaxis = list(
        title = "Time",
        range = time_range,
        # Range selector with dropdown-style configuration
        rangeselector = list(
          buttons = list(
            list(
              count = 1,
              label = "1D",
              step = "day",
              stepmode = "backward"
            ),
            list(
              count = 7,
              label = "1W",
              step = "day",
              stepmode = "backward"
            ),
            list(
              count = 1,
              label = "1M",
              step = "month",
              stepmode = "backward"
            ),
            list(step = "all", label = "All")
          )
        ),
        # Range slider at the bottom
        rangeslider = list(
          visible = TRUE,
          range = time_range
        )
      ),
      yaxis = list(
        title = "Pressure",
        side = "left"
      ),
      dragmode = "select",
      selectdirection = "d", # Prevent horizontal/vertical only selection
      showlegend = FALSE,
      # Add margins to provide space for the right y-axis and range controls
      margin = list(
        l = 60, # Left margin for pressure y-axis
        r = 80, # Right margin for acceleration y-axis (increased)
        t = 80, # Top margin (increased for range selector)
        b = 60 # Bottom margin (increased for range slider)
      ),
      # Performance optimizations
      hovermode = "closest"
    )

    # Add yaxis2 if acceleration data exists
    if (has_acceleration) {
      layout_config$yaxis2 <- list(
        title = "Acceleration",
        side = "right",
        overlaying = "y",
        position = 1,
        fixedrange = FALSE,
        scaleanchor = NULL
      )
    }

    p_with_layout <- do.call(layout, c(list(p), layout_config))

    p_with_layout |>
      config(
        scrollZoom = FALSE, # Disable built-in scroll zoom
        displayModeBar = TRUE,
        doubleClick = "reset", # Reset zoom on double click
        modeBarButtonsToRemove = list(
          "zoomIn2d",
          "zoomOut2d",
          "autoScale2d",
          "resetScale2d",
          "toImage",
          "hoverClosestCartesian",
          "hoverCompareCartesian",
          "select2d", # Remove box select tool
          "lasso2d" # Remove lasso select tool
        ),
        displaylogo = FALSE
      ) |>
      # Setup plotly event handlers for selection and click with Ctrl/Cmd key detection
      htmlwidgets::onRender(
        "
        function(el, x) {
          setupPlotlyEventHandlers(el);
        }
      "
      )
  })

  # Handle active series changes and label updates without resetting view
  # Use debounced reactive to prevent excessive updates
  observeEvent(
    {
      list(
        input$active_series,
        reactive_label_pres(),
        reactive_label_acc()
      )
    },
    {
      # Apply styling using the external function
      apply_plot_styling(
        plotlyProxy("ts_plot", session),
        input$active_series,
        reactive_label_pres(),
        reactive_label_acc(),
        session
      )
    }
  )

  # Shared labeling function for both selection and click events
  apply_labels_to_points <- function(point_data, ctrl_pressed = FALSE) {
    active_series <- input$active_series

    # Use empty string if Ctrl/Cmd is pressed, otherwise use selected label
    selected_label <- if (isTRUE(ctrl_pressed)) "" else input$label_select

    # Handle empty data - do nothing silently
    if (is.null(point_data) || nrow(point_data) == 0 || length(point_data) == 0) {
      return()
    }

    # Treat pressure line clicks as pressure markers for labeling
    point_data$curveNumber[point_data$curveNumber == 0] <- 1

    # Filter points to keep only those from the active series
    # Determine which curveNumber corresponds to the active series
    has_acceleration <- !is.null(acceleration_data)
    target_curve <- if (active_series == "pressure") {
      1 # Pressure markers are curveNumber 1
    } else if (active_series == "acceleration" && has_acceleration) {
      2 # Acceleration is curveNumber 2
    } else {
      NULL
    }

    # Filter points to keep only those from the active series
    if (!is.null(target_curve)) {
      point_data <- point_data[point_data$curveNumber == target_curve, , drop = FALSE]
    }

    # Check if any points remain after filtering
    if (nrow(point_data) == 0) {
      showNotification(
        glue::glue(
          "No {active_series} points in selection. Switch active series or select different points."
        ),
        duration = 3,
        type = "warning"
      )
      return()
    }

    # Apply labels to the filtered points
    point_indices <- point_data$pointNumber + 1 # Convert to 1-based indexing

    if (active_series == "pressure") {
      current_labels <- reactive_label_pres()
      current_labels[point_indices] <- selected_label
      reactive_label_pres(current_labels)
    } else if (active_series == "acceleration") {
      current_labels <- reactive_label_acc()
      current_labels[point_indices] <- selected_label
      reactive_label_acc(current_labels)
    }

    # Show success notification with appropriate message
    label_action <- if (isTRUE(ctrl_pressed)) {
      "Cleared labels from"
    } else {
      glue::glue("Applied label '{selected_label}' to")
    }
    showNotification(
      glue::glue("{label_action} {length(point_indices)} {active_series} points"),
      duration = 2,
      type = "message"
    )
  }

  # Helper function to process plotly events (selection/click)
  process_plotly_event <- function(event_info) {
    if (!is.null(event_info$points) && length(event_info$points) > 0) {
      # Convert to data frame format expected by apply_labels_to_points
      point_data <- data.frame(
        pointNumber = sapply(event_info$points, function(p) {
          if (is.null(p$pointNumber)) 0 else p$pointNumber
        }),
        curveNumber = sapply(event_info$points, function(p) {
          if (is.null(p$curveNumber)) 0 else p$curveNumber
        }),
        x = sapply(event_info$points, function(p) if (is.null(p$x)) NA else p$x),
        y = sapply(event_info$points, function(p) if (is.null(p$y)) NA else p$y),
        stringsAsFactors = FALSE
      )
      apply_labels_to_points(point_data, event_info$ctrlPressed)
    }
    apply_plot_styling(
      plotlyProxy("ts_plot", session),
      input$active_series,
      reactive_label_pres(),
      reactive_label_acc(),
      session
    )
  }

  # Handle point selection events with direct key state
  observeEvent(input$plotly_selected_with_keys, {
    process_plotly_event(input$plotly_selected_with_keys)
  })

  # Handle point click events with direct key state
  observeEvent(input$plotly_click_with_keys, {
    process_plotly_event(input$plotly_click_with_keys)
  })

  # Handle export button - use downloadHandler for file save dialog
  output$export_btn <- downloadHandler(
    filename = function() {
      # Remove any existing file extension from tag ID before adding our suffix
      base_name <- tools::file_path_sans_ext(tag$param$id)
      glue::glue("{base_name}-labeled.csv")
    },
    content = function(file) {
      tryCatch(
        {
          # Update tag with current reactive labels
          tag$pressure$label <- reactive_label_pres()
          if (has_acceleration) {
            tag$acceleration$label <- reactive_label_acc()
          }

          # Call tag_label_write to export the labels to the selected file
          tag_label_write(tag, file = file, quiet = TRUE)
        },
        error = function(e) {
          # Show error notification
          showNotification(
            glue::glue("Export failed: {e$message}"),
            duration = 10,
            type = "error"
          )
        }
      )
    },
    contentType = "text/csv"
  )
}
