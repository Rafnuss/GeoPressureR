#' Request and download mismatch maps of pressure
#'
#' This function return the mismatch map of atmospheric pressure measured by a
#' geolocator (`PAM_data`). It performs the following actions: (1) Send a query
#' to produce the Google Earth Engine (GEE) url of the code producing the maps
#' for each stationary periods separately, (2) then read these map (geotiff) in
#' a raster and (3) compute the likelihood map from the mismatch
#'
#' @param pressure pressure list from PAM logger dataset list (see https://github.com/KiranLDA/PAMLr for details).
#' @param extent Geographical extend of the map to query as a list ordered by West,East,South,North  (e.g. c(-6,43,0,47))
#' @param scale Number of pixel per latitude, longitude. 10 for a resoltion of 0.1° (~10) and 4 for a resolution of 0.25° (~30km). To avoid interpolating the ERA5 data, scale should be smaller than 10. Read more about scale on Google earth Engine documention.
#' @param maxSample The computation of the mismatch is only performed on `maxSample` datapoints of pressure to reduce computational time. The samples are randomly (uniformly) selected on the timeserie.
#' @param margin The margin is used in the threshold map to accept some measurement error. unit in meter. (1hPa~10m)
#' @return List of raster map
#' @export
geopressure.map =
  function(pressure,
           extent,
           scale = 10,
           maxSample = 250,
           margin = 30) {
    # Check input
    testthat::expect_type(pressure, "list")
    testthat::expect_true("date" %in% names(pressure))
    testthat::expect_is(pressure$date, "POSIXt")
    testthat::expect_true("obs" %in% names(pressure))
    testthat::expect_is(pressure$obs, c("integer", 'numeric'))
    testthat::expect_true("class" %in% names(pressure))
    testthat::expect_is(pressure$class, "logical")
    testthat::expect_true("staID" %in% names(pressure))
    testthat::expect_length(pressure$obs, length(pressure$date))
    testthat::expect_length(pressure$class, length(pressure$date))
    testthat::expect_length(pressure$staID, length(pressure$date))
    testthat::expect_is(extent, c("integer", 'numeric'))
    testthat::expect_length(extent, 4)
    testthat::expect_true(extent[1] >= -180 & extent[1] <= 180)
    testthat::expect_true(extent[2] >= -180 & extent[2] <= 180)
    testthat::expect_true(extent[3] >= -90 & extent[3] <= 90)
    testthat::expect_true(extent[4] >= -90 & extent[4] <= 90)
    testthat::expect_true(extent[1] < extent[2])
    testthat::expect_true(extent[3] < extent[4])
    testthat::expect_is(scale, c("integer", 'numeric'))
    testthat::expect_gt(scale, 0)
    testthat::expect_lte(scale, 10)
    testthat::expect_is(maxSample, c("integer", 'numeric'))
    testthat::expect_gt(maxSample, 0)
    testthat::expect_is(margin, c("integer", 'numeric'))
    testthat::expect_gte(margin, 0)

    # convert from hPa to Pa
    pres = pressure$obs * 100


    # remove outliar as labeled in TRAINSET
    pres[!is.na(pressure$class)] = NA

    # remove flight period
    pres[pressure$staID == 0] = NA

    # smooth the data to 1hr
    # need to be done

    # downscale to 1hour
    pres[format(pressure$date, "%M") != "00"] = NA

    # remove stationary period with NA
    pres[is.na(pressure$staID)] = NA

    # Format query
    bodyDF = list(
      time = jsonlite::toJSON(as.numeric(as.POSIXct(pressure$date[!is.na(pres)]))),
      label = jsonlite::toJSON(pressure$staID[!is.na(pres)]),
      pressure = jsonlite::toJSON(pres[!is.na(pres)]),
      W = extent[1],
      S = extent[3],
      E = extent[2],
      N = extent[4],
      scale = scale,
      maxSample = maxSample,
      margin = margin
    )

    # Request URLS
    message("Sending requests...")
    res =
      httr::POST("http://glp.mgravey.com:24853/GeoPressure/v1/map/", body = bodyDF) #httr::verbose()

    # check that the response is successful
    if (!httr::content(res)$status == "success") {
      message(httr::content(res))
      stop('Error with request')
    } else {
      # Get URIS
      # print(httr::content(res))
      uris = unlist(httr::content(res)$data$urls)
      labels = unlist(httr::content(res)$data$labels)
      message(
        'Request generated successfully for ',
        length(labels),
        ' stationary periods (',
        sprintf('%d, ', sort(labels)),
        ')'
      )
    }

    # Perform the call in parallel
    # GEE allows up to 12 requests at the same time, so we set the worker to 10
    future::plan(future::multisession, workers = 10)
    f = c()
    message("Starting download:")
    for (i_u in 1:length(uris)) {
      f[[i_u]] = future::future({
        raster::brick(uris[i_u])
      }, seed = TRUE)
      progress(i_u, max = length(uris))
    }

    # Get the raster
    rasterList = c()
    message("Receiving download (geotiff):")
    for (i_u in 1:length(uris)) {
      rasterList[[i_u]] = future::value(f[[i_u]])
      progress(i_u, max = length(uris))

      # convert MSE from Pa to hPa
      rasterList[[i_u]][[1]] = rasterList[[i_u]][[1]] / 100 / 100

      # Writing some metadata
      raster::metadata(rasterList[[i_u]]) = list(
        staID = labels[i_u],
        nbSample = sum(bodyDF$label == labels[i_u]),
        maxSample = maxSample,
        extendSample = c(min(pressure$date[!is.na(pres) &
                                             pressure$staID == labels[i_u]]), max(pressure$date[!is.na(pres) &
                                                                                                  pressure$staID == labels[i_u]])),
        margin = margin
      )
    }

    # return
    rasterList
  }








#' Convert MSE to probability
#'
#' This function
#'
#' @param rasterList list of raster loaded from `geopressure.map()`
#' @param s standardeviation of the pressure error
#' @param thr threashold of the percentage of datapoint outside the elevation range to be considered not possible
#' @return List of the probability raster map
#' @export
geopressure.Probmap = function(rasterList, s = 1, thr = 0.9) {
  rasterProbList = c()
  for (i_s in 1:length(rasterList)) {
    # get metadata
    mt = raster::metadata(rasterList[[i_s]])

    # compute Log-linear pooling weight
    # pres_n = min(nbSample, mt$maxSample)
    pres_n = as.numeric(difftime(mt$extendSample[2], mt$extendSample[1], units = "hours"))
    w = log(pres_n) - 1

    # get MSE layer
    rasterProbList[[i_s]]  = rasterList[[i_s]][[1]]
    # change 0 (water) in NA
    rasterProbList[[i_s]][rasterProbList[[i_s]] == 0] = NA
    # compute probability with equation
    rasterProbList[[i_s]] =  exp(-w * rasterProbList[[i_s]] / (s ^ 2))
    # mask value of threashold
    rasterProbList[[i_s]] = rasterProbList[[i_s]] * (rasterList[[i_s]][[2]] > thr)

    # plot(rasterProbList[[i_s]])
    raster::metadata(rasterProbList[[i_s]]) = raster::metadata(rasterList[[i_s]])
  }
  rasterProbList
}






#' Request and download surface pressure timeseries at location
#'
#' This function return the surfrace atmospheric pressure timeseries from ERA5
#' at a particualy location specify by lat and lon. I uses SRTM-30 to translate
#' the pressure for the exact elevation of the ground level, accounting for
#' both temporal varation of pressure and temperature.
#'
#' If you supply the pressure (and time) of the geolocator, it will additionally
#' return the elevation of the geolocator above sea level considering that the bird was located at the location specify
#'
# The timeserie of the response will be on the same as time if supply, otherwise, it will return on a hourly basis between `startTime` and `endTime`.
#'
#' @param lon longitude to query (-180° to 180°).
#' @param lat latitude to query (0° to 90°).
#' @param pressure pressure list from PAM logger dataset list (see https://github.com/KiranLDA/PAMLr for details).
#' @param startTime if pressure not provided, then the starttime of the timeserie return is needed
#' @param endTime same as starttime
#' @return Timeserie of date, pressure and optionally altitude
#' @export
geopressure.timeseries =
  function(lon,
           lat,
           pressure = NULL,
           endTime = NULL,
           startTime = NULL) {
    #Test
    testthat::expect_is(lon, "numeric")
    testthat::expect_is(lat, "numeric")
    testthat::expect_true(lon >= -180 & lon <= 180)
    testthat::expect_true(lat >= -90 & lat <= 90)
    if (!is.null(pressure)) {
      testthat::expect_type(pressure, "list")
      testthat::expect_true("date" %in% names(pressure))
      testthat::expect_is(pressure$date, "POSIXt")
      testthat::expect_true("obs" %in% names(pressure))
      testthat::expect_is(pressure$obs, c("numeric", 'integer'))
      testthat::expect_length(pressure$obs, length(pressure$date))
      endTime = NULL
      startTime = NULL
    } else {
      testthat::expect_is(endTime, "POSIXt")
      testthat::expect_is(startTime, "POSIXt")
      testthat::expect_gt(endTime, startTime)
    }

    # Format query
    bodyDF = list(lon = lon,
                   lat = lat)
    if (!is.null(pressure)) {
      bodyDF$time = jsonlite::toJSON(as.numeric(as.POSIXct(pressure$date)))
      bodyDF$pressure = jsonlite::toJSON(pressure$obs * 100)
    } else {
      bodyDF$startTime = jsonlite::toJSON(as.numeric(as.POSIXct(startTime)))
      bodyDF$endTime = jsonlite::toJSON(as.numeric(as.POSIXct(endTime)))
    }

    # Request URLS
    message("Sending requests...")
    res =
      httr::POST("http://glp.mgravey.com:24853/GeoPressure/v1/timeseries",
                 body = bodyDF) #httr::verbose()

    # check that the response is successful
    if (!httr::content(res)$status == "success") {
      print(httr::content(res))
      stop('Error with request')
    } else {
      message('Request generated successfully.')
    }
    message('Downloading csv data.')
    out = utils::read.csv(httr::content(res)$data$url)

    # convert Pa to hPa
    out$pressure = out$pressure / 100

    # convert time into date
    out$time = as.POSIXct(out$time, origin = "1970-01-01")
    names(out)[names(out) == 'time'] = 'date'

    # return
    out
  }






# Progress bar function
progress = function (x, max = 100) {
  percent = x / max * 100
  cat(sprintf('\r[%-50s] %d / %d',
              paste(rep('=', percent / 2), collapse = ''),
              x, max))
  if (x == max)
    cat('\n')
}
