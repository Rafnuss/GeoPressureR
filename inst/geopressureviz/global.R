# remove.packages("GeoPressureR")
# devtools::install_github("rafnuss/GeoPressureR")

list.of.packages <- c("shiny", "shinyjs", "shinyWidgets")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages) > 0) install.packages(new.packages)

suppressMessages({
  # library(BiocManager)
  # options(repos = BiocManager::repositories())
  library(GeoPressureR)
  library(shinyjs)
  library(shiny)
  library(shinyWidgets)
  # library(RColorBrewer)
})
