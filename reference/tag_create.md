# Create a `tag` object

Create a GeoPressureR `tag` object from the data collected by a tracking
device. The function can read data formatted according to three
manufacturers SOI, Migratetech or Lund CAnMove, as well as according to
the GeoLocator Data package standard and also accept manual data.frame
as input. Pressure data is required for the GeoPressureR workflow but
can be allowed to be missing with `assert_pressure = FALSE`.

## Usage

``` r
tag_create(
  id,
  manufacturer = NULL,
  crop_start = NULL,
  crop_end = NULL,
  directory = glue::glue("./data/raw-tag/{id}"),
  pressure_file = NULL,
  light_file = NULL,
  acceleration_file = NULL,
  temperature_external_file = NULL,
  temperature_internal_file = NULL,
  magnetic_file = NULL,
  assert_pressure = TRUE,
  quiet = FALSE
)
```

## Arguments

- id:

  unique identifier of a tag.

- manufacturer:

  One of `NULL`, `"soi"`, `"migratetech"`, `"bas"`, `"lund"`,
  `"prestag"`, `"datapackage"` or `"dataframe"`.

- crop_start:

  remove all data before this date (POSIXct or character in UTC).

- crop_end:

  remove all data after this date (POSIXct or character in UTC).

- directory:

  path of the directory where the tag files can be read.

- pressure_file:

  name of the file with pressure data. Full pathname or finishing with
  extensions (e.g., `"*.pressure"`, `"*.deg"` or `"*_press.xlsx"`).

- light_file:

  name of the file with light data. Full pathname or finishing with
  extensions (e.g., `"*.glf"`, `"*.lux"` or `"*_acc.xlsx"`).

- acceleration_file:

  name of the file with acceleration data. Full pathname or finishing
  with extensions (e.g., `"*.acceleration"`, `"*.deg"` or
  `"*_acc.xlsx"`).

- temperature_external_file:

  name of the file with temperature data. Full pathname or finishing
  with extensions (e.g., `"*.temperature"`, `"*.airtemperature"` or
  `"*.deg"`). External or air temperature is generally for temperature
  sensor on directed outward from the bird.

- temperature_internal_file:

  name of the file with temperature data . Full pathname or finishing
  with extensions (e.g., `"*.bodytemperature"`). Internal or body
  temperature is generally for temperature sensor on directed inward
  (between bird and tag).

- magnetic_file:

  name of the file with magnetic/accelerometer data. Full pathname or
  finishing with extensions (e.g., `"*.magnetic"`).

- assert_pressure:

  logical to check that the return tag has pressure data.

- quiet:

  logical to hide messages about the progress.

## Value

a GeoPressureR `tag` object containing

- `param` parameter object (see
  [`param_create()`](https://raphaelnussbaumer.com/GeoPressureR/reference/param_create.md))

- `pressure` data.frame with columns: `date` and `value`

- `light` (optional) same structure as pressure

- `temperature_external` (optional) same structure as pressure

- `temperature_internal` (optional) same structure as pressure

- `acceleration` (optional) data.frame with columns: `date`, `value`,
  `act` and `pit`.

  - `value` is the activity computed as the sum of the difference in
    acceleration on the z-axis (i.e. jiggle). In the SOI sensor, it is
    summarised from 32 measurements at 10Hz

  - `pitch` is the relative position of the bird’s body relative to the
    z axis. In the SOI sensor, it is an average over 32 measurements at
    10Hz.

- `magnetic` (optional) data.frame with columns: `date`, `magnetic_x`,
  `magnetic_y`, `magnetic_z` , `acceleration_x`, `acceleration_y` and
  `acceleration_z`

## Details

The current implementation can read files from the following three
sources:

- [GeoLocator Data Package
  (`gldp`)](https://raphaelnussbaumer.com/GeoLocator-DP/)

  - `pressure_file = "pressure.csv"`(optional)

  - `light_file = "light.csv"` (optional)

  - `acceleration_file = "acceleration.csv"` (optional)

  - `temperature_external_file = "temperature_external.csv"` (optional)

  - `temperature_external_file = "temperature_external.csv"` (optional)

  - `magnetic_file = "magnetic.csv"` (optional)

- [Swiss Ornithological Institute (`soi`)](https://bit.ly/3QI6tkk)

  - `pressure_file = "*.pressure"`

  - `light_file = "*.glf"` (optional)

  - `acceleration_file = "*.acceleration"` (optional)

  - `temperature_internal_file = "*.temperature"` (optional)

  - `temperature_external_file = "*.airtemperature"` (optional)

  - `magnetic_file = "*.magnetic"` (optional)

- [Migrate Technology (`migratetech`)](https://www.migratetech.co.uk/):

  - `pressure_file = "*.deg"`

  - `light_file = "*.lux"` (optional)

  - `acceleration_file = "*.deg"` (optional)

- British Antarctic Survey (`bas`), acquired by Biotrack Ltd in 2011,
  [renamed Lotek in 2019](https://www.lotek.com/about-us/history/) .
  Only works for light data (`assert_pressure = FALSE`)

  - `light_file = "*.lig"`

- [Lund CAnMove (`lund`)](https://bit.ly/3P6quyi)

  - `pressure_file = "*_press.xlsx"`

  - `light_file = "*_acc.xlsx"` (optional)

  - `acceleration_file = "*_acc.xlsx"` (optional)

- [BitTag/PresTag
  (`prestag`)](https://geoffreymbrown.github.io/ultralight-tags/)

  - `pressure_file = "*.txt"`

You can also enter the data manually (`manufacturer = "dataframe"`) by
providing the data.frame:

- `pressure_file`: data.frame with columns `date` and `value` in hPa.

- `light_file`: (optional) data.frame with columns `date` and `value`.

- `acceleration_file`: (optional) data.frame with columns `date` and
  `value`.

- `temperature_external_file`: (optional) data.frame with columns `date`
  and `value`.

- `temperature_internal_file`: (optional) data.frame with columns `date`
  and `value`.

- `magnetic_file`: (optional) data.frame with columns `date`,
  `magnetic_x`, `magnetic_y`, `magnetic_z`, `acceleration_x`,
  `acceleration_y` and `acceleration_z`.

You can still create a `tag` without pressure data using
`assert_pressure = TRUE`. This `tag` won't be able to run the
traditional GeoPressureR workflow, but you can still do some analysis.

By default `manufacturer = NULL`, the manufacturer is determined
automatically from the content of the `directory`. You can also specify
manually the file with a full pathname or the file extension using a
regex expression (e.g., `"*.pressure"` matches any file ending with
`pressure`).

Please create [an issue on
Github](https://github.com/Rafnuss/GeoPressureR/issues/new) if you have
data in a format that is not yet supported.

This function can be used to crop the data at specific date, for
instance to remove pre-equipment or post-retrieval data.

## See also

[GeoPressureManual](https://bit.ly/4462jpr)

Other tag:
[`print.tag()`](https://raphaelnussbaumer.com/GeoPressureR/reference/print.tag.md),
[`tag_set_map()`](https://raphaelnussbaumer.com/GeoPressureR/reference/tag_set_map.md)

## Examples

``` r
withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
  # Read all sensor file
  tag <- tag_create("18LX")

  print(tag)

  # Read only pressure and crop date
  tag <- tag_create("18LX",
    light_file = NULL,
    acceleration_file = NULL,
    crop_start = "2017-08-01",
    crop_end = "2017-08-05"
  )

  print(tag)

  # You can also specify the exact file in case multiple files with the
  # same extension exist in your directory (migratetech data)
  tag <- tag_create("CB621",
    pressure_file = "CB621_BAR.deg",
    light_file = "CB621.lux",
    acceleration_file = NULL
  )

  print(tag)

  # You can specify the data manually with
  pressure <- data.frame(
    date = as.POSIXct(c(
      "2017-06-20 00:00:00 UTC", "2017-06-20 01:00:00 UTC",
      "2017-06-20 02:00:00 UTC", "2017-06-20 03:00:00 UTC"
    ), tz = "UTC"),
    value = c(1000, 1000, 1000, 1000)
  )
  tag_create(id = "xxx", pressure_file = pressure)
})
#> ✔ Read ./data/raw-tag/18LX/18LX_20180725.pressure
#> ✔ Read ./data/raw-tag/18LX/18LX_20180725.glf
#> ✔ Read ./data/raw-tag/18LX/18LX_20180725.acceleration
#> ✔ Read ./data/raw-tag/18LX/18LX_20180725.temperature
#> 
#> ── GeoPressureR `tag` object for 18LX ──────────────────────────────────────────
#> Note: All green texts are fields of `tag` (i.e., `tag$field`).
#> 
#> ── Parameter param 
#> Run `tag$param` to display all parameters
#> 
#> ── Sensors data 
#> Manufacturer: soi
#> Date range: 2017-06-20 to 2017-08-09 23:55:00
#> • pressure: 672 datapoints (30min)
#> • acceleration: 4,032 datapoints (5min)
#> • light: 4,032 datapoints (5min)
#> • temperature_external: 2,448 datapoints (30min)
#> 
#> ── Stationary periods stap 
#> ✖ No stationary periods defined yet. Use `tag_label()`
#> ✔ Read ./data/raw-tag/18LX/18LX_20180725.pressure
#> ✔ Read ./data/raw-tag/18LX/18LX_20180725.glf
#> ✔ Read ./data/raw-tag/18LX/18LX_20180725.acceleration
#> ✔ Read ./data/raw-tag/18LX/18LX_20180725.temperature
#> 
#> ── GeoPressureR `tag` object for 18LX ──────────────────────────────────────────
#> Note: All green texts are fields of `tag` (i.e., `tag$field`).
#> 
#> ── Parameter param 
#> Run `tag$param` to display all parameters
#> 
#> ── Sensors data 
#> Manufacturer: soi
#> Date range: 2017-08-01 to 2017-08-04 23:55:00
#> • pressure: 192 datapoints (30min)
#> • acceleration: 1,152 datapoints (5min)
#> • light: 1,152 datapoints (5min)
#> • temperature_external: 192 datapoints (30min)
#> 
#> ── Stationary periods stap 
#> ✖ No stationary periods defined yet. Use `tag_label()`
#> ✔ Read ./data/raw-tag/CB621/CB621_BAR.deg
#> ✔ Read ./data/raw-tag/CB621/CB621_BAR.deg
#> ✔ Read ./data/raw-tag/CB621/CB621.lux
#> 
#> ── GeoPressureR `tag` object for CB621 ─────────────────────────────────────────
#> Note: All green texts are fields of `tag` (i.e., `tag$field`).
#> 
#> ── Parameter param 
#> Run `tag$param` to display all parameters
#> 
#> ── Sensors data 
#> Manufacturer: migratetech
#> Date range: 2021-06-15 21:13:35 to 2021-06-16 05:08:35
#> • pressure: 16 datapoints (30min)
#> • light: 14 datapoints (5min)
#> 
#> ── Stationary periods stap 
#> ✖ No stationary periods defined yet. Use `tag_label()`
#> List of 2
#>  $ param   :List of 3
#>   ..$ id                  : chr "xxx"
#>   ..$ GeoPressureR_version:Classes 'package_version', 'numeric_version'  hidden list of 1
#>   .. ..$ : int [1:3] 3 4 5
#>   ..$ tag_create          :List of 3
#>   .. ..$ pressure_file: chr "df"
#>   .. ..$ manufacturer : chr "df"
#>   .. ..$ directory    : chr "(not used)"
#>   ..- attr(*, "class")= chr "param"
#>  $ pressure:'data.frame':    4 obs. of  2 variables:
#>   ..$ date : POSIXct[1:4], format: "2017-06-20 00:00:00" "2017-06-20 01:00:00" ...
#>   ..$ value: num [1:4] 1000 1000 1000 1000
#>  - attr(*, "class")= chr "tag"
```
