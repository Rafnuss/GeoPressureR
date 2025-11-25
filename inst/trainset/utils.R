library(shiny)
library(plotly)

# Utility functions for GeoPressure trainset Shiny app

# Function to apply plot styling based on active series and labels
apply_plot_styling <- function(
  plot_or_proxy,
  active_series,
  label_pres,
  label_acc = NULL,
  session = NULL
) {
  print(paste("Applying plot styling for active series:", active_series))
  # Calculate colors once for both series
  pressure_colors <- get_marker_colors(label_pres)

  # Define styling for pressure LINE trace (trace 0 - for range slider)
  pressure_line_style <- if (active_series == "pressure") {
    list(
      "line.color" = "black",
      "line.width" = 2
    )
  } else {
    list(
      "line.color" = "rgba(0,0,0,0.2)",
      "line.width" = 1
    )
  }

  # Define styling for pressure MARKERS trace (trace 1 - for interaction)
  pressure_markers_style <- list(
    "marker.size" = if (active_series == "pressure") 10 else 4,
    "marker.opacity" = if (active_series == "pressure") 0.8 else 0.4,
    "marker.color" = list(pressure_colors) # Wrap in list for plotly array
  )

  # Check if this is initial plot creation or proxy update
  if (!is.null(session)) {
    plot_or_proxy |>
      plotlyProxyInvoke("restyle", list(selectedpoints = NULL))

    # This is a proxy update (reactive styling change)
    # Apply styling to pressure LINE trace (trace 0)
    plot_or_proxy |>
      plotlyProxyInvoke("restyle", pressure_line_style, list(0))

    # Apply styling to pressure MARKERS trace (trace 1)
    plot_or_proxy |>
      plotlyProxyInvoke("restyle", pressure_markers_style, list(1))

    # Apply styling to acceleration series (trace 2) if it exists
    if (!is.null(label_acc)) {
      acceleration_colors <- get_marker_colors(label_acc)
      acceleration_style <- list(
        "marker.size" = if (active_series == "acceleration") 10 else 5,
        "marker.opacity" = if (active_series == "acceleration") 0.8 else 0.3,
        "marker.color" = list(acceleration_colors),
        "line.color" = if (active_series == "acceleration") {
          "black"
        } else {
          "rgba(0,0,0,0.2)"
        },
        "line.width" = if (active_series == "acceleration") 2 else 1
      )
      plot_or_proxy |>
        plotlyProxyInvoke("restyle", acceleration_style, list(2))
    }
  } else {
    # This is initial plot creation - return styles for use in plot creation
    result <- list(
      pressure_line_style = pressure_line_style,
      pressure_markers_style = pressure_markers_style
    )

    if (!is.null(label_acc)) {
      acceleration_colors <- get_marker_colors(label_acc)
      acceleration_style <- list(
        "marker.size" = if (active_series == "acceleration") 10 else 5,
        "marker.opacity" = if (active_series == "acceleration") 0.8 else 0.3,
        "marker.color" = list(acceleration_colors),
        "line.color" = if (active_series == "acceleration") {
          "black"
        } else {
          "rgba(0,0,0,0.2)"
        },
        "line.width" = if (active_series == "acceleration") 2 else 1
      )
      result$acceleration_style <- acceleration_style
    }

    return(result)
  }
}

# Define color mapping function for plot markers
get_marker_colors <- function(labels) {
  # Base colors for special cases
  colors <- ifelse(
    is.na(labels) | labels == "",
    "black", # No label = black
    ifelse(
      labels == "discard",
      "grey", # Discard = grey
      ifelse(labels == "flight", "red", "")
    )
  ) # Flight = red

  # Get unique additional labels (excluding NA, "", "discard", "flight")
  additional_labels <- unique(labels[
    !is.na(labels) & labels != "" & labels != "discard" & labels != "flight"
  ])

  # Nice color palette for additional categories (colorbrewer-inspired)
  # Excludes red (flight) and grey (discard) colors
  category_palette <- c(
    "#1f77b4", # blue
    "#ff7f0e", # orange
    "#2ca02c", # green
    "#9467bd", # purple
    "#8c564b", # brown
    "#e377c2", # pink
    "#bcbd22", # olive
    "#17becf" # cyan
  )

  # Assign colors to additional labels
  if (length(additional_labels) > 0) {
    for (i in seq_along(additional_labels)) {
      label <- additional_labels[i]
      color <- category_palette[((i - 1) %% length(category_palette)) + 1]
      colors[labels == label & !is.na(labels)] <- color
    }
  }

  colors
}
