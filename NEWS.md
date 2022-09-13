# GeoPressureR v2.6-beta
## Major
- add windspeed download function `graph_download_wind()`

## Minor
- fixes for reading pam data
- various fixes (see https://github.com/Rafnuss/GeoPressureR/pull/42)

## Full Changelog 
https://github.com/Rafnuss/GeoPressureR/compare/v2.5-beta...v2.6-beta

# GeoPressureR v2.5-beta
## Major
- Migration of all the vignette and data used for the vignette in GeoPressureManual bda0f7898dd9e6b8d9d786ce56ae3e5ec422c935
- Read Migrate Technology data (should not be breaking change, but some significant changes) #23
- Add `logis` function in `flight_prob()` 6e1a8f0e93d82ec2a9bccce404cdb59fcc218277

## Minor
- Read Avonet data as package data c5c8d807f9a7e13a49e3d1565a7b3beffb58022f
- Update of `r-lib/actions` to v2 3382fb9b7b9970f1c102cf9aabf3a6b06b5d505e
- 8720b6e6032f910f0c702e649a907dcf10bc2258
- Improvement of GeoPressureViz 97be49de4ed6c309b16e23fbedde1d618ae0a04c 964b558913de7f7b6ef9915fc9cc41fc0b3dd0d3
- Add checks and warning in functions
- Preparation of the code for CRAN

## Full Changelog
https://github.com/Rafnuss/GeoPressureR/compare/v2.4-beta...v2.5-beta

# GeoPressureR v2.4-beta
## Major
- Accept request over water and display warning message. See https://github.com/Rafnuss/GeoPressureR/pull/15
- Add logging of error and return JSON file of the request in case of error for troubleshooting 
- Change downloading and reading of geotiff file to work on windows. See https://github.com/Rafnuss/GeoPressureR/issues/16
- Remove the artificial increase of flight duration at the creation of graph https://github.com/Rafnuss/GeoPressureR/commit/696566e8041e90d04e3e01d7d84ef299660bab6e
- Compute groundspeed in parallel in graph creation https://github.com/Rafnuss/GeoPressureR/commit/b1466c737a66c740e2f6a35bcdbc19d9f5aebfd1

## Minor
- minor fixes for `sta_id=0` or `NA`
- minor fixes in geopressureviz
- add dummy graph test to improve coverage.
- compute windspeed for short flight happening during the same hour
- typos, code readability and stlyer

## Full Changelog
https://github.com/Rafnuss/GeoPressureR/compare/v2.3-beta...v2.4-beta


# GeoPressureR v2.3-beta
## Major
- [Major fix of wind computation bearing to angle and m/s -> km/h](https://github.com/Rafnuss/GeoPressureR/commit/0eee443944e0b7ecf86c64901b45cd0f659d3d19)
- Major fix of twilight uncertainty using kernel density. The gamma fitting was very wrong https://github.com/Rafnuss/GeoPressureR/commit/5acfb136b8cac49d3cfd9633ce9a0a81ccc9b252
- Major update in the data location to avoid being loaded when using the package. Move all data to `inst/extdata` to avoid having them loaded with https://github.com/Rafnuss/GeoPressureR/commit/65c8f8062cf07fb1471c9f15f6f08757d00951df
- Add more information on various dataset to be able to load in GeoPressureViz
- Change to the graph https://github.com/Rafnuss/GeoPressureR/commit/4aeed9ab77c8efe15b2da591247700d0ebb0cb5f

## Minor
- Multiple test file and add `covr`
- [Optimize `sta_pam()`](https://github.com/Rafnuss/GeoPressureR/commit/eb398697ce600d229f14c50141808ab671c1309d)
- [Re-write `find_twilights`](https://github.com/Rafnuss/GeoPressureR/commit/d52e14e62c4b212a54f31ca78baa5342d372b4c7)
- [Create function graph_path2edge](https://github.com/Rafnuss/GeoPressureR/commit/db73fcfea317f0db795d6a629bec9d42b9f073fd)
- [Add energy figure](https://github.com/Rafnuss/GeoPressureR/commit/8b4c4efbce7c029e1a0f0628985bf53616c829a0)
- Multiple improvement on GeoPressureViz
- Add citation and contribution file
- [use 100 character width](https://github.com/Rafnuss/GeoPressureR/commit/ae97874788658b8684bb3e3fa539c063cc0046ab)
- Add link to [GeoPressureTemplate](https://github.com/Rafnuss/GeoPressureTemplate)

## Full Changelog
https://github.com/Rafnuss/GeoPressureR/compare/v2.2-beta...v2.3-beta


# GeoPressureR v2.2-beta
## Major
- New function `geopressure_map2path` with return of index of lat-lon option
- New function `geopressure_ts_path` to compute mulitple `geopressure_ts` function on a full path
- Update GeoPressureViz (https://rafnuss.shinyapps.io/GeoPressureViz/) to accept `geopressure_ts_path` output

## Minor
- fix flight and avonet databse (https://github.com/Rafnuss/GeoPressureR/issues/10)
- fix https://github.com/Rafnuss/GeoPressureR/issues/9 

## Full Changelog
https://github.com/Rafnuss/GeoPressureR/compare/v2.1-beta...v2.2-beta
 
 
# GeoPressureR v2.1-beta
## Major
- Graph Addition of wind: https://raphaelnussbaumer.com/GeoPressureR/articles/wind-graph.html
- Movement model function: converting airspeed/groundspeed to probability.

## Minor
- Minor correction of existing code
- cleaning of name, variable and file saved for more consistency
- Update to GeoPresureAPI v2.1

## Full Changelog 
https://github.com/Rafnuss/GeoPressureR/compare/v2.0-beta...v2.1-beta


# GeoPressureR v2.0-beta
## What's Changed
* Add vignette and code for light geopositioning by @Rafnuss in https://github.com/Rafnuss/GeoPressureR/pull/4
* minor language changes by @jsocolar in https://github.com/Rafnuss/GeoPressureR/pull/7

## New Contributors
* @jsocolar made their first contribution in https://github.com/Rafnuss/GeoPressureR/pull/7

## Full Changelog 
https://github.com/Rafnuss/GeoPressureR/compare/v1.1-beta...v2.0-beta


# GeoPressureR v1.1-beta
## Full Changelog 
https://github.com/Rafnuss/GeoPressureR/commits/v1.1-beta
