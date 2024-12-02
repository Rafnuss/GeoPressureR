# GeoPressureR v3.3.2

## Major update
* Change the computation of distance of edges in the graph by removing the fix that added 1 resolution to the distance to account for large grid square and short flight distance. Instead, add warning message in case there might be such an issue (flight distance < grid resolution) [ddbd07d](https://github.com/Rafnuss/GeoPressureR/pull/126/commits/ddbd07db195440005f7f1ff96dd134dd43bdd8ae). 
* Add `zero_speed_threshold` parameter that allow to encourage bird to stay at the same location. This is typically the case for short flight that don't seems to affect the position. Quite similar to a `stap_elev` [2060790](https://github.com/Rafnuss/GeoPressureR/pull/126/commits/2060790f8508c447a00877c2fc1d86c9d52bfe2e)
* Add other type of pressurepath in interim [8d7f1d1](https://github.com/Rafnuss/GeoPressureR/pull/126/commits/8d7f1d12665608d37f5eafa53fb251554e7f5cdd)

## Minor
* Plenty of small fixes and minor improvements [834155e](https://github.com/Rafnuss/GeoPressureR/pull/126/commits/834155eeffcf04b33effce8d9b9d193c5554cbdf), [152c6e7](https://github.com/Rafnuss/GeoPressureR/pull/126/commits/152c6e7a2091970b444a47a3ceb0a367a42a9c2a), [54aa995](https://github.com/Rafnuss/GeoPressureR/pull/126/commits/54aa99589f8e709089a08ae02c5aace821ab7cea),  [3a5ff95](https://github.com/Rafnuss/GeoPressureR/pull/126/commits/3a5ff95774b353c853a32ed63327dad8df82fbbe), [6eedd83](https://github.com/Rafnuss/GeoPressureR/pull/126/commits/6eedd83802f9377db43cba06c2e4b684b2154452), [8797533](https://github.com/Rafnuss/GeoPressureR/pull/126/commits/8797533b49c9cc76a3d81a90824a8ceaabd48692), [1e68260](https://github.com/Rafnuss/GeoPressureR/pull/126/commits/1e68260c39bc49816c95095d0f3c3541986a50df)

**Full Changelog**: https://github.com/Rafnuss/GeoPressureR/compare/v3.3.1...v3.3.2


# GeoPressureR v3.3.1

## Major update
* [Update of param structure to function_name$param_name](https://github.com/Rafnuss/GeoPressureR/commit/2235e9ec0bceef3b49d8e1887a309b2048353552). The structure of the param has been reorganised: this named list stored inside of tag and graph stores parameters used during the building of tag and graph. We standardized this structure as param${function_name}${argument_name}. [See the migration instructions in the GeoPressureR wiki](https://github.com/Rafnuss/GeoPressureR/wiki/Migration-v3.x-%E2%80%90--v3.3). This will mean you'll need to update your config.yml structure - sorry for that.
* [Add geopressuretemplate() functions](https://github.com/Rafnuss/GeoPressureR/commit/b000764ff3c2179eefb48bbf9178c5323c89aa7d). The main improvement is related to the use of a single function to run the entire workflow: geopressuretemplate. Read more about this in the [corresponding chapter of the GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/geopressuretemplate-workflow.html).

## Minor
* [replace species_name by scientific_name](https://github.com/Rafnuss/GeoPressureR/commit/5e1b15fd025f355107a90229779969ad2030d7c7)
* [crop date UTC](https://github.com/Rafnuss/GeoPressureR/commit/0707ae6041383bff704a4c78511ab2dbd16305ce)
* [fix new variable name in netcdf](https://github.com/Rafnuss/GeoPressureR/commit/6c12362128c569c05ae49bb0ed40b8a31adc5980)

## New Contributors
* @PabloCapilla made their first contribution in https://github.com/Rafnuss/GeoPressureR/pull/125

**Full Changelog**: https://github.com/Rafnuss/GeoPressureR/compare/v3.3.0...v3.3.1

# GeoPressureR v3.3

## Major
- Read all sensors type and allow reading sensor without pressure `assert_pressure = FALSE` (https://github.com/Rafnuss/GeoPressureR/pull/123/commits/d11f8cc4774b4c91c27318c43d438823b681066e, https://github.com/Rafnuss/GeoPressureR/pull/123/commits/85ffe940ec46af8cd56592c2d641f25d19712129)
- [Update to ecmwfr v2. Change to cds_token](https://github.com/Rafnuss/GeoPressureR/pull/123/commits/4253e04f9ed16e3b06d45edfda0b2a0900d31d0c). We use the Climate Data Store to download the wind data during the flight. They have recently [updated their infrastructure](https://confluence.ecmwf.int/display/CKB/Please+read%3A+CDS+and+ADS+migrating+to+new+infrastructure%3A+Common+Data+Store+%28CDS%29+Engine) and their login procedure has changed. You’ll need an ECMF login with an Access Token. See updated procedure in the chapter [Trajectory with wind of the GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/trajectory-with-wind.html#download-wind-data).
- [Improvement of tag_label_auto() with post-processing step](https://github.com/Rafnuss/GeoPressureR/commit/69c26adf559fc1bc7c2690346de41f6732f9eda5). Based on a simple classification of prolonged high activity, migratory flight classification was often not very performant, e.g. when a bird was gliding during the flight. I have now added a post-processing step in the automatic classification to fix this. Read more in [the detail section of tag_label_auto().](https://raphaelnussbaumer.com/GeoPressureR/reference/tag_label_auto.html#details).
- [Create path2twilight.R](https://github.com/Rafnuss/GeoPressureR/commit/eff97315c5eca3dff03736e7d40efad30b209819) and [Add twilight_line in plot_twilight](https://github.com/Rafnuss/GeoPressureR/commit/b21aa06ceff257ec4473a3af530a8ba7cef5e225). You can now compute the theoretical twilight of a path, or more interestingly, of a pressurepath. It's also used in [pressurepath_create()](https://raphaelnussbaumer.com/GeoPressureR/reference/pressurepath_create.html), returning a column with sunrise and sunset. Its original purpose was to be able to check the twilight labeling by comparing it to a path generated, e.g., with GeoPressureviz. See the [last section of the light map chapter](https://raphaelnussbaumer.com/GeoPressureManual/light-map.html#check-light-label) for more info.

## Minor:
- [Fix issue with tag_plot_twilight() when twilight was not yet computed](https://github.com/Rafnuss/GeoPressureR/pull/123/commits/6de3af2a97e27763c5a70a0e570c91921b699f01)
- [Update documentation of windsupport/drift](https://github.com/Rafnuss/GeoPressureR/pull/123/commits/93ec8a579e12fb39fe034d108f8afa89150e700d)
- [Make twilight works with NA in light](https://github.com/Rafnuss/GeoPressureR/pull/123/commits/3a704fd054127cbc72e303091a4755bcbd31eaf0)
- [Add type to path as attribute](https://github.com/Rafnuss/GeoPressureR/pull/123/commits/e900e518d39a0e1b44a98f98b81aa1ffef17c760)
- [Accept known as list and convert it if so](https://github.com/Rafnuss/GeoPressureR/pull/123/commits/22307d41014da8c2ddfbd10861078f89b7426451)
- [Fix bug in compute_known in geolight_map()](https://github.com/Rafnuss/GeoPressureR/pull/123/commits/e4b02796a8932f014aaa0fc212631d1dd24c8a52)
- Improve progress_bar, remove extra `\f`, improve `print` 
- [Change default map height](https://github.com/Rafnuss/GeoPressureR/pull/123/commits/7fc311be6cacafef9c3954700f1deb68f97c77b2)
- [fix plot_twilight() for twl_offset](https://github.com/Rafnuss/GeoPressureR/commit/2734537e7997555200f05eb30b451520e1c1cfb7)
- [update all actions](https://github.com/Rafnuss/GeoPressureR/commit/076b3568d191e60d96a3a4703b7709a947dae66c)

## Full Changelog

[https://github.com/Rafnuss/GeoPressureR/compare/v3.2.0...v3.3.0](https://github.com/Rafnuss/GeoPressureR/compare/v3.2.0...v3.3.0)

# GeoPressureR v3.2
## Major
- Use the new GeoPressureAPI pressurepath entry point for `geopressurepath_create()`
- Update to GeoPressureAPI v2 for `geopressure_timeseries()` https://github.com/Rafnuss/GeoPressureR/pull/103/commits/732d1a02cc241dc7d2dde3401c8747fa860650c6
- Fix major bug https://github.com/Rafnuss/GeoPressureR/pull/103/commits/05c3203ef1588bbc1f769050377cadf5f1aadcbd
- Migrate from `httr`to `httr2`
- Fix major bug with saving environment variable in param https://github.com/Rafnuss/GeoPressureR/commit/9bcbf790a7de133448c738db272bf136dc831f8f
- Add functions `speed2bearing()` and `windsupport()` https://github.com/Rafnuss/GeoPressureR/commit/fe244f6057db14b6a286c0a77aaaef4c5ec0152c
- Use interpolated `stap_id` for flight instead of `0` https://github.com/Rafnuss/GeoPressureR/commit/d7491c2a5580c9eeafe598935933727489590a75
- Create `edge_add_wind()` https://github.com/Rafnuss/GeoPressureR/commit/36412dc8f0fd061798a701ca632766dbe6f069c8
- Create `path2elevation()` using GeoPressureAPI to compute ground elevation from a path

## Minor
- Add `workers` argument in `graph_create()` https://github.com/Rafnuss/GeoPressureR/commit/e1ce45882809e1fd3da0e8feb2ff80ac70f2bf8b
- Add `codemeta.json` https://github.com/Rafnuss/GeoPressureR/pull/105/commits/4f7f7bce8875b4af59db3fc1ce403d41d6317469
- Add project status badge https://github.com/Rafnuss/GeoPressureR/pull/105/commits/ecd8f61ec49dcd376748e54d19dfb2000675d302
- Fix leaflet tile provider with Stadia change https://github.com/Rafnuss/GeoPressureR/pull/106/commits/8d9bd159874deb87d61907ea14911eca12877038
- Add `WORDLIST` for `spelling` package.
- Remove the use of `ind` in path https://github.com/Rafnuss/GeoPressureR/commit/f7b38e1c1b06666f590df394395d7db387f2565a
- Read temperature sensor https://github.com/Rafnuss/GeoPressureR/commit/17524658a5f49466a211ea5bfbfc34c523b09a47
- Only download wind data for non-existing file by default (instead of all flights) https://github.com/Rafnuss/GeoPressureR/commit/b6a2c41420bef2354a9ace640adffcb4e79e1aa1
- Remove `pressurepath2altitude()` now computed in `pressurepath_create()`

## Full Changelog

[https://github.com/Rafnuss/GeoPressureR/compare/v3.1.0...v3.2.0](https://github.com/Rafnuss/GeoPressureR/compare/v3.1.0...v3.2.0)


# GeoPressureR v3.1
## Major
* Update to GeoPressureAPI v2, using `thr_mask` in `geopressure_map_mismatch()` and splitting `keep_mse_mask`.
* Adjust computation of ground speed to account for grid resolution. 

## Minor
* Use negative indexing for `known`
* remove trailing `/` to default directories.
* documentations and minor fixes.

## Full Changelog
[https://github.com/Rafnuss/GeoPressureR/compare/v3.0.0...v3.1.0](https://github.com/Rafnuss/GeoPressureR/compare/v3.0.0...v3.1.0)


# GeoPressureR v3.0

## Guiding principles of v3
This new version consists of a significant revamp of the entire code centred around these themes:

- Name more general than SOI sensors (e.g., use `tag` instead of `pam`)
- Focus the workflow on pressure sensor (but still allows for acceleration or light data)
- Update the notion of graph into State-Space Model notations (e.g. probability -> likelihood)
- More memory efficient (store minimum graph info) while minimizing computational expense of the "slow" functions
- Shorter workflow [#69](https://github.com/Rafnuss/GeoPressureR/issues/69)
- Ease of labelling [#67](https://github.com/Rafnuss/GeoPressureR/issues/67)
- Reproducibility and long-term storage with `param`.
- Use of S3 class object with print and plot generic function.
- Compatible with pipe `|>` or `%>%`
- Use of [cli](https://cli.r-lib.org/index.html) for message and progress bar.
- Be able to update `tag` and `pressurepath` without re-computing everything.

⚠️ See [#55](https://github.com/Rafnuss/GeoPressureR/issues/55) for details on the functions named change
⚠️ See the [migration wiki](https://github.com/Rafnuss/GeoPressureR/wiki/Migration-v2-%E2%80%90--v3) for a small guide to transition from v2.

## Major
* Use of GeoPressureR object: `tag`, `graph`, `param`, `bird`
* Many new plot functions including update of `geopressureviz()`
* Transition from `raster` to `terra` #59
* New label scheme with test and messaging for troubleshooting #67 #73 #83
* Create `tag_update()` and `pressurepath_update()`
* Review the structure of a path and edges. 

## Minor

* Formulate graph as a HMM #68
* Simplified workflow #69
* Use of `cli` for message. 
* Create `graph_shortestpath` https://github.com/Rafnuss/GeoPressureR/commit/b69c2a21b784f598b03822e940c02c216114e9f9
* Review all tests and example
* Review all functions names and parameters

# GeoPressureR v2.7-beta
## Major
* Major fix in the computation of the marginal map https://github.com/Rafnuss/GeoPressureR/commit/bd1103fda0c5b4e3c0f218ee7bcf3fbc69dc6123

## Minor
* Improve `graph_download_wind()` #54 
* GeoPressureViz function in #52
* Replace `isoutliar` with `isoutlier` in #43
* Use `assertthat` in #46 and #47
* Typo of equipment and retrieval in #48
* Various minor fixes

## Full Changelog
[https://github.com/Rafnuss/GeoPressureR/compare/v2.6-beta...v2.7-beta](https://github.com/Rafnuss/GeoPressureR/compare/v2.6-beta...v2.7-beta)

# GeoPressureR v2.6-beta
## Major
- add windspeed download function `graph_download_wind()`

## Minor
- fixes for reading pam data
- various fixes (see #42)

## Full Changelog 
[https://github.com/Rafnuss/GeoPressureR/compare/v2.5-beta...v2.6-beta](https://github.com/Rafnuss/GeoPressureR/compare/v2.5-beta...v2.6-beta)

# GeoPressureR v2.5-beta
## Major
- Migration of all the vignette and data used for the vignette in GeoPressureManual https://github.com/Rafnuss/GeoPressureR/commit/bda0f7898dd9e6b8d9d786ce56ae3e5ec422c935
- Read Migrate Technology data (should not be breaking change, but some significant changes) #23
- Add `logis` function in `flight_prob()` https://github.com/Rafnuss/GeoPressureR/commit/6e1a8f0e93d82ec2a9bccce404cdb59fcc218277

## Minor
- Read Avonet data as package data https://github.com/Rafnuss/GeoPressureR/commit/c5c8d807f9a7e13a49e3d1565a7b3beffb58022f
- Update of `r-lib/actions` to v2 https://github.com/Rafnuss/GeoPressureR/commit/3382fb9b7b9970f1c102cf9aabf3a6b06b5d505e
- https://github.com/Rafnuss/GeoPressureR/commit/8720b6e6032f910f0c702e649a907dcf10bc2258
- Improvement of GeoPressureViz https://github.com/Rafnuss/GeoPressureR/commit/97be49de4ed6c309b16e23fbedde1d618ae0a04c https://github.com/Rafnuss/GeoPressureR/commit/964b558913de7f7b6ef9915fc9cc41fc0b3dd0d3
- Add checks and warning in functions
- Preparation of the code for CRAN

## Full Changelog
[https://github.com/Rafnuss/GeoPressureR/compare/v2.4-beta...v2.5-beta](https://github.com/Rafnuss/GeoPressureR/compare/v2.4-beta...v2.5-beta)

# GeoPressureR v2.4-beta
## Major
- Accept request over water and display warning message. See #15
- Add logging of error and return JSON file of the request in case of error for troubleshooting 
- Change downloading and reading of geotiff file to work on windows. See #16
- Remove the artificial increase of flight duration at the creation of graph https://github.com/Rafnuss/GeoPressureR/commit/696566e8041e90d04e3e01d7d84ef299660bab6e
- Compute groundspeed in parallel in graph creation https://github.com/Rafnuss/GeoPressureR/commit/b1466c737a66c740e2f6a35bcdbc19d9f5aebfd1

## Minor
- minor fixes for `sta_id=0` or `NA`
- minor fixes in `geopressureviz()`
- add dummy graph test to improve coverage.
- compute windspeed for short flight happening during the same hour
- typos, code readability and `stlyer`

## Full Changelog
[https://github.com/Rafnuss/GeoPressureR/compare/v2.3-beta...v2.4-beta](https://github.com/Rafnuss/GeoPressureR/compare/v2.3-beta...v2.4-beta)


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
[https://github.com/Rafnuss/GeoPressureR/compare/v2.2-beta...v2.3-beta](https://github.com/Rafnuss/GeoPressureR/compare/v2.2-beta...v2.3-beta)


# GeoPressureR v2.2-beta
## Major
- New function `geopressure_map2path` with return of index of lat-lon option
- New function `geopressure_ts_path` to compute multiple `geopressure_ts` function on a full path
- Update GeoPressureViz (https://rafnuss.shinyapps.io/GeoPressureViz/) to accept `geopressure_ts_path` output

## Minor
- fix flight and avonet database #10
- fix #9 

## Full Changelog
[https://github.com/Rafnuss/GeoPressureR/compare/v2.1-beta...v2.2-beta](https://github.com/Rafnuss/GeoPressureR/compare/v2.1-beta...v2.2-beta)

 
# GeoPressureR v2.1-beta
## Major
- Graph Addition of wind: https://raphaelnussbaumer.com/GeoPressureR/articles/wind-graph.html
- Movement model function: converting airspeed/groundspeed to probability.

## Minor
- Minor correction of existing code
- cleaning of name, variable and file saved for more consistency
- Update to GeoPressureAPI v2.1

## Full Changelog 
[https://github.com/Rafnuss/GeoPressureR/compare/v2.0-beta...v2.1-beta](https://github.com/Rafnuss/GeoPressureR/compare/v2.0-beta...v2.1-beta)


# GeoPressureR v2.0-beta
## What's Changed
* Add vignette and code for light geopositioning in #4
* minor language changes by @jsocolar in #7

## New Contributors
* @jsocolar made their first contribution in #7

## Full Changelog 
[https://github.com/Rafnuss/GeoPressureR/compare/v1.1-beta...v2.0-beta](https://github.com/Rafnuss/GeoPressureR/compare/v1.1-beta...v2.0-beta)


# GeoPressureR v1.1-beta
## Full Changelog 
https://github.com/Rafnuss/GeoPressureR/commits/v1.1-beta
