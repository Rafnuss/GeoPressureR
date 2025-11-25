list_of_packages <- c("shiny", "plotly", "bslib")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages)
}

suppressMessages({
  library(GeoPressureR)
  library(shiny)
  library(plotly)
  library(bslib)
})
