library(testthat)
library(GeoPressureR)

skip("skip tag_download_wind")

# Set working directory
setwd(system.file("extdata", package = "GeoPressureR"))

Sys.setenv(cds_user = "15631", cds_key = "381a49e9-7c3a-45e9-9b9a-d4ca16b3ed0e")

tag <- tag_create("18LX")
tag <- tag_label(tag)

expect_error(tag_download_wind(tag))

tag <- tag_set_map(tag,
                   extent = c(-16, 23, 0, 50),
                   scale = 2,
                   known = data.frame(
                     stap_id = 1,
                     known_lon = 17.05,
                     known_lat = 48.9
                   )
)

# tmp = tag_download_wind(tag, variable = c("u", "temperature"), include_stap_id = 1)

g <- map_expand(tag$param$extent, tag$param$scale)
flight <- stap2flight(tag$stap, format = "list", include_stap_id = 1)
file = \(stap_id) glue::glue("./data/wind/{tag$param$id}/{tag$param$id}_{stap_id}.nc")

GeoPressureR:::edge_add_wind_check(file, flight, tag$pressure, g)

tag_download_wind(tag)


