#' Retrieve an ERA5 pressure time series from ARCO
#'
#' This is the explicit ARCO backend for [geopressure_timeseries()]. See the parent function for
#' the shared workflow, backend comparison, ECMWF key setup, and output details.
#'
#' @inheritParams geopressure_timeseries
#' @return See [geopressure_timeseries()].
#' @family pressurepath
#' @export
geopressure_timeseries_arco <- function(
  lat,
  lon,
  pressure = NULL,
  start_time = NULL,
  end_time = NULL,
  quiet = FALSE,
  debug = FALSE,
  era5_dataset = c("land", "single-levels")
) {
  geopressure_timeseries(
    lat = lat,
    lon = lon,
    pressure = pressure,
    start_time = start_time,
    end_time = end_time,
    source = "arco",
    quiet = quiet,
    debug = debug,
    era5_dataset = era5_dataset
  )
}

geopressure_timeseries_arco_impl <- function(
  lat,
  lon,
  pressure = NULL,
  start_time = NULL,
  end_time = NULL,
  quiet = FALSE,
  debug = FALSE,
  era5_dataset = c("land", "single-levels")
) {
  era5_dataset <- match.arg(era5_dataset)
  dataset_name <- if (era5_dataset == "land") "ERA5-Land" else "ERA5 single levels"
  resolution <- if (era5_dataset == "land") 0.1 else 0.25
  cache_dir <- tools::R_user_dir("GeoPressureR", "cache")
  query_lon <- floor(lon / resolution + 0.5) * resolution
  query_lat <- floor(lat / resolution + 0.5) * resolution

  arco_client <- era5_arco_client()

  if (!is.null(pressure)) {
    requested_date <- as.POSIXct(pressure$date, tz = "UTC")
    first_hour <- ceiling(min(as.numeric(requested_date)) / 3600)
    requested_hour <- as.POSIXct(
      pmax(first_hour, ceiling(as.numeric(requested_date) / 3600 - 0.5)) * 3600,
      origin = "1970-01-01",
      tz = "UTC"
    )
    date <- seq(
      min(requested_hour),
      max(requested_hour),
      by = "hour"
    )
  } else {
    date <- seq(
      as.POSIXct(ceiling(as.numeric(start_time) / 3600) * 3600, origin = "1970-01-01", tz = "UTC"),
      as.POSIXct(ceiling(as.numeric(end_time) / 3600) * 3600, origin = "1970-01-01", tz = "UTC"),
      by = "hour"
    )
  }

  if (!quiet) {
    cli::cli_progress_step("Read {dataset_name} pressure from the ECMWF ARCO archive")
  }
  surface_pressure <- era5_arco_read(
    variable = "sp",
    era5_dataset = era5_dataset,
    lon = query_lon,
    lat = query_lat,
    date = date,
    arco_client = arco_client,
    debug = debug
  )

  # The ARCO archive is masked over oceans. Match GeoPressureAPI by moving an ocean point inland.
  if (era5_dataset == "land" && all(is.na(surface_pressure))) {
    lsm_file <- file.path(cache_dir, "lsm_1279l4_0.1x0.1.grb")
    if (!file.exists(lsm_file)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
      httr2::request(
        "https://confluence.ecmwf.int/download/attachments/140385202/lsm_1279l4_0.1x0.1.grb?version=1&modificationDate=1567528624201&api=v2"
      ) |>
        httr2::req_perform(path = lsm_file)
    }
    land <- terra::rast(lsm_file)
    terra::ext(land) <- c(-0.05, 359.95, -90.05, 90.05)
    land <- terra::rotate(land)
    radius <- 10
    radius_lon <- min(180, radius / abs(cos(lat * pi / 180)))
    candidate <- terra::crop(
      land,
      terra::ext(lon - radius_lon, lon + radius_lon, lat - radius, lat + radius)
    )
    candidate_cell <- which(terra::values(candidate) > 0.5, arr.ind = FALSE)
    xy <- terra::xyFromCell(candidate, candidate_cell)
    dlon <- (xy[, 1] - lon) * pi / 180
    dlat <- (xy[, 2] - lat) * pi / 180
    a <- sin(dlat / 2)^2 + cos(lat * pi / 180) * cos(xy[, 2] * pi / 180) * sin(dlon / 2)^2
    distance <- 6371000 * 2 * atan2(sqrt(a), sqrt(1 - a))
    nearest <- which.min(distance)
    lon <- unname(xy[nearest, 1])
    lat <- unname(xy[nearest, 2])
    query_lon <- round(lon * 10) / 10
    query_lat <- round(lat * 10) / 10
    if (!quiet) {
      cli::cli_alert_warning(
        "Requested position is over water; using the closest ERA5-Land cell ({round(distance[nearest] / 1000)} km away)."
      )
    }
    surface_pressure <- era5_arco_read(
      variable = "sp",
      era5_dataset = era5_dataset,
      lon = query_lon,
      lat = query_lat,
      date = date,
      arco_client = arco_client,
      debug = debug
    )
  }

  out <- data.frame(
    date = date,
    surface_pressure = surface_pressure / 100,
    lat = lat,
    lon = lon
  )

  if (!is.null(pressure)) {
    nearest_time <- match(requested_hour, date)
    out <- out[nearest_time, ]
    out$date <- requested_date

    if (!quiet) {
      cli::cli_progress_step("Read {dataset_name} temperature and compute altitude")
    }
    temperature <- era5_arco_read(
      variable = "t2m",
      era5_dataset = era5_dataset,
      lon = query_lon,
      lat = query_lat,
      date = date,
      arco_client = arco_client,
      debug = debug
    )
    elevation <- era5_surface_elevation(query_lon, query_lat, era5_dataset, quiet)
    out$altitude <- pressure_to_altitude(
      pressure$value * 100,
      surface_pressure[nearest_time],
      temperature[nearest_time],
      elevation
    )
    out <- out[c("date", "surface_pressure", "altitude", "lat", "lon")]

    pressure_merge <- pressure
    if (!("stap_id" %in% names(pressure_merge))) {
      pressure_merge$stap_id <- 1
    }
    if (!("label" %in% names(pressure_merge))) {
      pressure_merge$label <- ""
    }
    out <- merge(pressure_merge, out, all.x = TRUE)
    names(out)[names(out) == "value"] <- "pressure_tag"
    stap_id <- out$stap_id
    label <- out$label
    id_norm <- stap_id != 0 & label != "discard"
    if (sum(id_norm) > 0) {
      elev <- ifelse(startsWith(label, "elev_"), gsub("^.*?elev_", "", label), "0")
      for (elev_i in unique(elev)) {
        id_elev <- elev == elev_i
        out$surface_pressure_norm[id_elev] <- out$surface_pressure[id_elev] -
          mean(out$surface_pressure[id_elev & id_norm]) +
          mean(out$pressure_tag[id_elev & id_norm])
      }
    }
  }

  return(out)
}

era5_arco_read <- function(
  variable,
  era5_dataset,
  lon,
  lat,
  date,
  arco_client,
  debug
) {
  store <- if (era5_dataset == "land") {
    switch(
      variable,
      sp = "cadl-arco-geo-009/arco/reanalysis_era5_land/sfc-pressure-precipitation",
      t2m = "cadl-arco-geo-007/arco/reanalysis_era5_land/sfc-2m-temperature"
    )
  } else {
    "cadl-arco-geo-002/arco/reanalysis_era5_single_levels/sfc"
  }
  array <- glue::glue(
    "https://arco.datastores.ecmwf.int/{store}/geoChunked.zarr/{variable}"
  )
  if (era5_dataset == "land") {
    time_index <- as.integer(as.numeric(date) / 3600 - (-175296) + 1)
    lat_index <- as.integer(round((lat + 90) * 10) + 1)
    lon_index <- if (lon == -180) 3600L else as.integer(round((lon + 179.9) * 10) + 1)
  } else {
    time_index <- as.integer((as.numeric(date) - (-946771200)) / 3600 + 1)
    lat_index <- as.integer(round((lat + 90) * 4) + 1)
    lon_index <- if (lon == 180) 1L else as.integer(round((lon + 180) * 4) + 1)
  }
  if (debug) {
    cli::cli_text(
      "Read {.field {variable}} indexes {range(time_index)}, {lat_index}, {lon_index} from {.url {array}}"
    )
  }
  Rarr::read_zarr_array(
    array,
    index = list(time_index, lat_index, lon_index),
    s3_client = arco_client
  ) |>
    drop() |>
    unname()
}

era5_arco_client <- function(cache = FALSE) {
  cds_token <- ecmwfr::wf_get_key()
  arco_host <- "https://arco.datastores.ecmwf.int"
  object_cache <- new.env(parent = emptyenv())
  list(
    get_object = function(Bucket, Key, ...) {
      cache_key <- glue::glue("{Bucket}/{Key}")
      if (cache && exists(cache_key, envir = object_cache, inherits = FALSE)) {
        body <- get(cache_key, envir = object_cache, inherits = FALSE)
      } else {
        body <- httr2::request(glue::glue("{arco_host}/{Bucket}/{Key}")) |>
          httr2::req_auth_bearer_token(cds_token) |>
          httr2::req_perform() |>
          httr2::resp_body_raw()
        if (cache) {
          assign(cache_key, body, envir = object_cache)
        }
      }
      list(Body = body)
    },
    list_objects_v2 = function(Bucket, Prefix, ...) {
      response <- httr2::request(glue::glue("{arco_host}/{Bucket}/{Prefix}")) |>
        httr2::req_method("HEAD") |>
        httr2::req_auth_bearer_token(cds_token) |>
        httr2::req_error(is_error = function(response) FALSE) |>
        httr2::req_perform()
      list(KeyCount = as.integer(httr2::resp_status(response) == 200L))
    }
  )
}
