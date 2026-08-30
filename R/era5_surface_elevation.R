era5_surface_elevation <- function(
  lon,
  lat,
  era5_dataset,
  quiet,
  cache_dir = tools::R_user_dir("GeoPressureR", "cache")
) {
  elevation <- rep(NA_real_, length(lon))
  for (dataset_i in unique(era5_dataset)) {
    id <- era5_dataset == dataset_i
    if (dataset_i == "land") {
      geopotential_file <- file.path(cache_dir, "geo_1279l4_0.1x0.1.grib2")
      if (!file.exists(geopotential_file)) {
        dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
        httr2::request(
          "https://confluence.ecmwf.int/download/attachments/140385202/geo_1279l4_0.1x0.1.grib2?version=1&modificationDate=1582901403445&api=v2"
        ) |>
          httr2::req_perform(path = geopotential_file)
      }
    } else {
      geopotential_file <- file.path(cache_dir, "era5-geopotential-0.25.grib")
      if (!file.exists(geopotential_file)) {
        dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
        ecmwfr::wf_request(
          list(
            dataset_short_name = "reanalysis-era5-single-levels",
            product_type = "reanalysis",
            variable = "geopotential",
            year = "2000",
            month = "01",
            day = "01",
            time = "00:00",
            data_format = "grib",
            download_format = "unarchived",
            target = basename(geopotential_file)
          ),
          path = cache_dir,
          verbose = !quiet
        )
      }
    }
    geopotential <- terra::rast(geopotential_file)
    terra::ext(geopotential) <- if (dataset_i == "land") {
      c(-0.05, 359.95, -90.05, 90.05)
    } else {
      c(-0.125, 359.875, -90.125, 90.125)
    }
    geopotential <- terra::rotate(geopotential)
    elevation[id] <- terra::extract(geopotential, cbind(lon[id], lat[id]))[[1]] / 9.80665
  }
  elevation
}
