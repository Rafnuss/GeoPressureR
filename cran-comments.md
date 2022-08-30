## R CMD check results
There were no ERRORs, WARNINGs or NOTEs locally.


There are 2 NOTEs on win-builder

* checking CRAN incoming feasibility ... NOTE
  Possibly misspelled words in DESCRIPTION:
    geolocators (10:106)
    
  Geolocators is not a misspelled word: https://en.wikipedia.org/wiki/Light_level_geolocator
  
* checking examples ... [15s] NOTE
  Examples with CPU (user + system) or elapsed time > 10s
                   user system elapsed
  geopressure_map 10.83   0.38   11.22
  
  I have try to cut the example to a minimum (pre-computing the map), but I can't get to run faster 
  See https://raphaelnussbaumer.com/GeoPressureR/reference/geopressure_map.html. Loading of the
  raster and sp package are taking all this time. 


There is1 ERROR on R-hub

* checking package dependencies ... ERROR
  Package required but not available: 'EBImage'
  
  Known issue from R-hub: https://github.com/r-hub/rhub/issues/471
  
  
## Downstream dependencies
This is a new package.
