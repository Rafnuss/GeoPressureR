

# appDir <- system.file("shiny-examples", "myapp", package = "GeoPressureR")
# appDir <- "./inst/geopressureviz/"
# source(paste0(appDir,'global'), local = TRUE)
# source(paste0(appDir,'ui.R'), local = TRUE)
# source(paste0(appDir,'server.R'), local = TRUE)

shinyApp(ui, server)
