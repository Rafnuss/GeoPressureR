#' @section ECMWF API key:
#' ARCO access and CDS downloads use the same ECMWF Personal Access Token. Create an ECMWF account,
#' copy the token from your [Climate Data Store profile](https://cds.climate.copernicus.eu/profile),
#' accept the dataset licence terms, and store the token once in your system keyring:
#'
#' ```r
#' ecmwfr::wf_set_key("your-personal-access-token")
#' ```
#'
#' Do not put the token in a script or commit it to version control. GeoPressureR retrieves the
#' stored token with [ecmwfr::wf_get_key()] for all ECMWF-backed functions, including
#' [geopressure_timeseries()], [pressurepath_create()], and [tag_download_wind()].
