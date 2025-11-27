# Download flight data

This function downloads data associated to each flight from the [ERA5
hourly pressure
levels](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-pressure-levels?tab=overview)
with the [Climate Data Store (CDS)](https://cds.climate.copernicus.eu/)
and through the [`ecmwfr` R
package](https://bluegreen-labs.github.io/ecmwfr/index.html).

[Any variable available from the ERA5 pressure
level](https://bit.ly/3BrwLBM) can be downloaded.

The flights are determined from the stationary periods classified
`tag$stap`. It requests a single file for each flight using the exact
time (hourly basis) and pressure (altitude). To make the download more
efficient,
[`wf_request_batch()`](https://bluegreen-labs.github.io/ecmwfr/articles/advanced_vignette.html#batch-parallel-requests)
is used to download all files at the same time (up to 20 requests in
parallel).

To be able to download data from the Climate Data Store (CDS), you will
need to create an ECMWF account on <https://www.ecmwf.int/>. Once
created, you can retrieve your API Token on
<https://cds.climate.copernicus.eu/profile> and save them in your local
keychain with:
` ecmwfr::wf_set_key("abcd1234-foo-bar-98765431-XXXXXXXXXX") `

More information [in the
GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/geopressuretemplate-wind.html).

## Usage

``` r
tag_download_wind(
  tag,
  extent = tag$param$tag_set_map$extent,
  include_stap_id = NULL,
  variable = c("u_component_of_wind", "v_component_of_wind"),
  file = function(stap_id, tag_id) {
    
    glue::glue("./data/wind/{tag_id}/{tag_id}_{stap_id}.nc")
 },
  overwrite = FALSE,
  workers = 19,
  cds_token = lifecycle::deprecated(),
  ...
)
```

## Arguments

- tag:

  a GeoPressureR `tag` object.

- extent:

  geographical extent of the map on which the likelihood will be
  computed. Vector of length 4 `c(xmin, xmax, ymin, ymax)` or
  `c(W, E, S, N)`.

- include_stap_id:

  stationary period identifiers of the start of the flight to download.
  Default is to download all flights.

- variable:

  list of variables to download from [the ERA5 pressure
  level](https://bit.ly/3BrwLBM) : `"u_component_of_wind"`,
  `"v_component_of_wind"`, `"temperature"`, `"fraction_of_cloud_cover"`,
  `"relative_humidity"`, `"vertical_velocity"`,
  `"specific_cloud_ice_water_content"`,
  `"specific_cloud_liquid_water_content"`, `"specific_humidity"`,
  `"specific_rain_water_content"`, `"specific_snow_water_content"`,
  `"divergence"`, `"geopotential"`, `"ozone_mass_mixing_ratio"`,
  `"potential_vorticity"`, `'vorticity"`.

- file:

  absolute or relative path of the ERA5 wind data file to be downloaded.
  Function taking as arguments (1) the stationary period identifier
  and (2) the tag_id.

- overwrite:

  logical. If `TRUE`, file is overwritten.

- workers:

  maximum number of simultaneous request that will be submitted to the
  service. Most ECMWF services are limited to 20 concurrent requests
  (default = 2).

- cds_token:

  **\[deprecated\]** Enter the API token with
  [`ecmwfr::wf_set_key()`](https://rdrr.io/pkg/ecmwfr/man/wf_set_key.html)

- ...:

  Arguments passed on to
  [`ecmwfr::wf_request_batch`](https://rdrr.io/pkg/ecmwfr/man/wf_request.html)

  `user`

  :   user (default = "ecmwf") provided by the ECMWF data service, used
      to retrieve the token set by
      [`wf_set_key`](https://rdrr.io/pkg/ecmwfr/man/wf_set_key.html)

  `path`

  :   path were to store the downloaded data

  `time_out`

  :   how long to wait on a download to start (default = `3600`
      seconds).

  `retry`

  :   polling frequency of submitted request for downloading (default =
      `30` seconds).

  `request_list`

  :   a list of requests that will be processed in parallel.

  `total_timeout`

  :   overall timeout limit for all the requests in seconds.

## Value

The path of the downloaded (requested file) or the an R6 object with
download/transfer information

## See also

[`wf_request_batch()`](https://bluegreen-labs.github.io/ecmwfr/reference/wf_request.html)
,
[GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/trajectory-with-wind.html)

Other movement:
[`bird_create()`](https://raphaelnussbaumer.com/GeoPressureR/reference/bird_create.md),
[`graph_set_movement()`](https://raphaelnussbaumer.com/GeoPressureR/reference/graph_set_movement.md),
[`graph_transition()`](https://raphaelnussbaumer.com/GeoPressureR/reference/graph_transition.md),
[`plot_graph_movement()`](https://raphaelnussbaumer.com/GeoPressureR/reference/plot_graph_movement.md),
[`speed2prob()`](https://raphaelnussbaumer.com/GeoPressureR/reference/speed2prob.md)
