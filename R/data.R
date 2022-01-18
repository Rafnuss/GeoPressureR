#' Mismatch map for 18LX
#'
#' This dataset contains the data generated with the GeoPressure API for the
#' vignettes
#'
#' `raster_list` was created with
#' @examples
#' \dontrun{
#' pam_data <- pam_read(system.file("extdata", package = "GeoPressureR"),
#'   crop_start = "2017-06-20", crop_end = "2018-05-02"
#' )
#' pam_data <- trainset_read(pam_data,
#' system.file("extdata", package = "GeoPressureR"))
#' pam_data <- pam_sta(pam_data)
#'
#' sta_id_keep <- pam_data$sta$sta_id[difftime(pam_data$sta$end,
#' pam_data$sta$start, units = "hours") > 12]
#' pam_data$pressure$sta_id[!(pam_data$pressure$sta_id %in% sta_id_keep)] <- NA
#'
#' raster_list <- geopressure_map(pam_data$pressure,
#' extent = c(-16, 20, 0, 50), scale = 10, max_sample = 100)
#'
#' # Save the data for vignette
#' usethis::use_data(raster_list, overwrite = T)
#' }
"raster_list"

#' Probability map for 18LX
#'
#' This dataset contains the data generated with the GeoPressure API for the
#' vignettes
#'
#' `prob_map_list` was created with
#' @examples
#' \dontrun{
#' prob_map_list <- geopressure_prob_map(raster_list)
#' # Save the data for vignette
#' usethis::use_data(prob_map_list, overwrite = T)
#' }
"prob_map_list"

#' Probability timeseries for 18LX
#'
#' This dataset contains the data generated with the GeoPressure API for the
#' vignette `basic_example.Rmd`and `labeling_tracks.Rmd`.
#'
#' `ts_list` was created with
#' @examples
#' \dontrun{
#' ts_list <- list()
#' for (i_r in 1:length(prob_map_list)) {
#'   i_s <- metadata(prob_map_list[[i_r]])$sta_id
#'
#'   # find the max value of probability
#'   tmp <- as.data.frame(prob_map_list[[i_r]], xy = T)
#'   lon <- tmp$x[which.max(tmp[, 3])]
#'   lat <- tmp$y[which.max(tmp[, 3])]
#'
#'   # query the pressure at this location
#'   message("query:", i_r, "/", length(sta_id_keep))
#'   ts_list[[i_r]] <- geopressure_ts(lon,
#'     lat,
#'     pressure = subset(pam_data$pressure, sta_id == 1)
#'   )
#'   # Add sta_id
#'   ts_list[[i_r]]["sta_id"] <- i_s
#'
#'   # Remove mean
#'   ts_list[[i_r]]$pressure0 <- ts_list[[i_r]]$pressure -
#'     mean(ts_list[[i_r]]$pressure) + mean(pam_data$pressure$obs[id])
#' }
#' # Save the data for vignette
#' usethis::use_data(ts_list, overwrite = T)
#' }
"ts_list"

#' Probability map of light for 18LX
#'
#' This dataset was generated with the vignette `Light-based geopositiong`
"raster_light_list"
