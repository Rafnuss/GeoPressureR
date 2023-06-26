library(testthat)
library(GeoPressureR)

# Hide cli message
options(cli.default_handler = function(...) { })

# Set working directory
setwd(system.file("extdata/", package = "GeoPressureR"))

tag <- tag_create(
  id = "18LX",
  crop_start = "2017-06-20", crop_end = "2018-05-02"
) |>
  tag_label()

tag <- tag_geostap(tag, c(-16, 23, 0, 50))
