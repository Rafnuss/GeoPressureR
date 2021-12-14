#' Download
#'
#' This function
#'
#' @param PAM_data PAM logger dataset list
#' @param file.location Path to the folder where the labeled files should be saved
#' @param file.name Name for the file (default is PAM_data$id)
#' @return PAM logger dataset list updated
#' @export
geopressure.request <- function(pressure, extent, scale=10, maxSample=250, margin=30){

  # Check input


  # convert from hPa to Ph
  pres = pressure$obs * 100;

  # remove outliar as labeled in TRAINSET
  pres[!is.na(pressure$class)] = NA

  # remove flight period
  pres[pressure$staID==0] = NA

  # smooth the data to 1hr

  # downscale to 1hour
  pres[format(pressure$date,"%M")!="00"] = NA

  # Format query
  bodyDF <- list(
    time = jsonlite::toJSON(as.numeric(as.POSIXct(pressure$date[!is.na(pres)]))),
    label = jsonlite::toJSON(pressure$staID[!is.na(pres)]),
    pressure = jsonlite::toJSON(pres[!is.na(pres)]),
    W=extent[1],
    S=extent[3],
    E=extent[2],
    N=extent[4],
    scale=scale,
    maxSample=maxSample,
    margin=margin
  )

  # Assert input


  # Request URLS
  print("Sending requests...",row.names = F)
  res <- httr::POST("http://glp.mgravey.com/GeoPressure/v1/map", body = bodyDF) #httr::verbose()

  # check that the response is successful
  if (!httr::content(res)$status=="success"){
    stop('Error with request')
  } else {
    # Get URIS
    uris = unlist(httr::content(res)$data$urls)
    print(paste0('Request generated successfully. ',length(uris),' URLs received.'),row.names = F)
  }

  # Perform the call in parrallel
  # GEE allows up to 12 requests at the same time, so we set the worker to 10
  future::plan(future::multisession, workers = 10)
  f = c()
  print("Starting download:",row.names = F)
  for (i_u in 1:length(uris)) {
    f[[i_u]] <- future::future({raster::stack(uris[i_u])},seed=TRUE)
    progress(i_u, max = length(uris))
  }

  # Get the raster
  v=c()
  print("Receiving download (geotiff):",row.names = F)
  for (i_u in 1:length(uris)) {
    v[[i_u]] <- future::value(f[[i_u]])
    progress(i_u, max = length(uris))

    # convert MSE from Pa to hPa
    values(v[[i_u]][[1]]) <- values(v[[i_u]][[1]]) / 100 / 100

    # compute number of datapoints
    pres_n = sum(pressure$staID[!is.na(pres)]==i_u)


    # Convert MSE in prob
    w = log(pres_n)-1;

    #
    s = 1

    #
    tmp <- v[[i_u]][[1]]
    values(tmp)[values(tmp)==0] <- NA
    values(tmp) <-  exp(-w*values(tmp)/(s^2))
    v[[i_u]] = addLayer(v[[i_u]], tmp)

    # Writing metadata
    metadata(v[[i_u]]) <- list(i_u,pres_n,w)
  }

  # return (such not intuitive to leave a variable out at the end to define what to return... )
  v
}


progress <- function (x, max = 100) {
  percent <- x / max * 100
  cat(sprintf('\r[%-50s] %d / %d',
              paste(rep('=', percent / 2), collapse = ''),
              x, max))
  if (x == max)
    cat('\n')
}
