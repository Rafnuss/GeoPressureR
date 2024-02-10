# remove.packages("GeoPressureR")
# devtools::install_github("rafnuss/GeoPressureR")

list_of_packages <- c("shiny", "shinyjs", "shinyWidgets")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) install.packages(new_packages)

suppressMessages({
  # library(BiocManager)
  # options(repos = BiocManager::repositories())
  library(GeoPressureR)
  library(shinyjs)
  library(shiny)
  library(shinyWidgets)
  # library(RColorBrewer)
})
