#' Create a `pressurepath`
#'
#' @description
#'
#' A `pressurepath` is a data.frame merging `path` and `tag$pressure`. It can be thought as the
#' equivalent of the measurement of variables by sensors equipped on the bird moving along a
#' specific `path`.
#'
#' Any ERA variables can be retrieved but the function is most notably used to retrieve ERA5
#' surface pressure to then be able to compare it to the pressure measured by the tag, and
#' potentially adjust labelling tag data.
#'
#' By default, We use both the [ERA5 LAND](https://doi.org/10.24381/cds.e2161bac) and
#' [ERA5 surface level](https://doi.org/10.24381/cds.adbb2d47) if position over water. Available
#' variables can be listed with `GeoPressureR:::pressurepath_variable` and details description
#' can be found on the [parameter listings in ERA5 doc](
#' https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation#heading-Parameterlistings).
#' Note that their exact name might be be different as we query them through Google Earth Engine.
#'
#' Positions during the flight are estimated by linear interpolation of the position of the stap
#' before and after. `pressurepath_create()` does not return measurement for stationary periods
#' which are not provided in `path` as well as the flight before and after.
#'
#'  also return the altitude of the bird along its trajectory. The altitude
#' \eqn{z_{tag}} (a.s.l.) is computed from the tag pressure \eqn{P_{tag}}, using the barometric
#' equation \deqn{ z_{{tag}}(x)=z_{ERA5}(x) + \frac{T_{ERA5}(x)}{L_b}  \left(
#' \frac{P_{tag}}{P_{ERA5}(x)} \right)^{\frac{RL_b}{g M}-1},}
#' where \eqn{z_{ERA}}, \eqn{T_{ERA}}, and \eqn{P_{ERA}} respectively correspond to the ground level
#' elevation, temperature at 2m, and ground level pressure of ERA5, \eqn{L_b}  is the standard
#' temperature lapse rate, \eqn{R} is the universal gas constant, \eqn{g} is the gravity constant
#' and  \eqn{M} is the molar mass of air. See more information at
#' [the GeoPressureAPI documentation](https://raphaelnussbaumer.com/GeoPressureAPI/#description-1).
#'
#' To be able to compare the temporal variation of the retrieved pressure of ERA5 \eqn{P_{ERA}} to
#' the tag pressure \eqn{P_{tag}}, the function also returns the ERA pressure normalized with
#' the tag mean pressure measurement as `surface_pressure_norm`.
#' \deqn{ P_{ERA5,0}(\boldsymbol{x})[t] = \left( P_{ERA5}(\boldsymbol{x})[t]-P_{tag}[t]\right) -
#' \left( \frac{1}{n}\sum_{i=1}^{n} P_{ERA5}(\boldsymbol{x})[i]-P_{tag}[i] \right).}
#'
#'  `pressurepath_create()` also computes the local sunrise and sunset times for each timestep
#'  according to the position of the path using `path2twilight()`. Sunrise and sunset are defined
#'  by the solar depression angle `solar_dep`.
#'
#' @param tag a GeoPressureR `tag` object.
#' @param path a GeoPressureR `path` data.frame.
#' @param variable ERA5 variable/parameters available to download.
#' The most commonly used variables:`"altitude"`, `"surface_pressure"`, `"temperature_2m"`,
#' `"u_component_of_wind_10m"`, `"v_component_of_wind_10m"`, `"u_component_of_wind_100m"`,
#' `"v_component_of_wind_100m"`, `"total_cloud_cover"`, `"total_precipitation_hourly"`,
#' `"land_sea_mask"`.
#' All variables can be listed with `GeoPressureR:::pressurepath_variable`.
#' @param solar_dep a numerical value representing the solar depression angle used to compute
#' sunrise and sunset. If `NULL`, does not compute sunrise sunset.
#' @param era5_dataset select the dataset to use: `"single-levels"` for [ERA5 hourly data on single
#' levels](https://doi.org/10.24381/cds.adbb2d47), `"land"` for [ERA5-Land hourly data](
#' https://doi.org/10.24381/cds.e2161bac) or `"both"` to use land where available and single-levels
#' otherwise (i.e. over water). LAND has greater precision but is not available on water. Using a
#' single one makes the query faster.
#' @param preprocess logical to pre-process pressure data with `geopressure_map_preprocess()`.
#' @param quiet logical to hide messages about the progress
#' @param workers number of parallel requests on GEE. Integer between 1 and 99.
#' @param debug logical to display additional information to debug a request
#'
#' @return A GeoPressureR `pressurepath` data.frame with columns:
#' - `date` same as `pressure$date`
#' - `stap_id` same as `pressure$stap_id`
#' - `pressure_tag` same as `pressure$value`
#' - `label` same as `pressure$label`
#' - `j` same as `path$j`
#' - `lat` same as `path$lat`
#' - `lon` same as `path$lon`
#' - `include` same as `path$include`
#' - `known` same as `path$known`
#' - `altitude` altitude of the bird along the path (see detail)
#' - `surface_pressure` pressure retrieved from ERA5.
#' - `surface_pressure_norm` pressure retrieved from ERA5 normalized to the average of
#' `pressure_tag` over the stationary period.
#' - `sunrise` datetime of the sunrise according to `solar_dep`.
#' - `sunset` datetime of the sunset according to `solar_dep`.
#' - `...` any other ERA5 variable requested by `variable`
#'
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |> tag_label(quiet = TRUE)
#' })
#'
#' path <- data.frame(
#'   stap_id = tag$stap$stap_id,
#'   lat = c(48.5, 32.5, 30.5, 49.5, 41.6),
#'   lon = c(17.5, 13.5, 16.5, 21.5, 12.7)
#' )
#'
#' pressurepath <- pressurepath_create(tag, path = path, quiet = TRUE)
#'
#' str(pressurepath)
#'
#' plot_pressurepath(pressurepath)
#'
#' pressurepath <- pressurepath_create(
#'   tag,
#'   path[c(2, 3, 4), ],
#'   quiet = TRUE
#' )
#'
#' plot_pressurepath(pressurepath)
#' @family pressurepath
#' @seealso [GeoPressureManual | Pressure
#' Map](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html)
#' @export
pressurepath_create <- function(tag,
                                path = tag2path(tag),
                                variable = c("altitude", "surface_pressure"),
                                solar_dep = 0,
                                era5_dataset = "both",
                                preprocess = FALSE,
                                workers = "auto",
                                quiet = FALSE,
                                debug = FALSE) {
  if (!quiet) {
    cli::cli_progress_step("Prepare pressure")
  }
  # Assert tag
  tag_assert(tag, "stap")

  # Validate requested variables against the allowed set
  unknown_vars <- setdiff(variable, pressurepath_variable)
  assertthat::assert_that(
    length(unknown_vars) == 0,
    msg = paste0(
      "Unknown variable(s): ", paste(unknown_vars, collapse = ", "),
      ". Allowed variables are: ", paste(pressurepath_variable, collapse = ", ")
    )
  )

  assertthat::assert_that(era5_dataset %in% c("single-levels", "land", "both"))

  # Assert preprocess
  assertthat::assert_that(is.logical(preprocess))
  if (preprocess) {
    pressure <- geopressure_map_preprocess(tag, compute_known = TRUE)
  } else {
    pressure <- tag$pressure
  }
  assertthat::assert_that(nrow(pressure) > 0)

  # Assert path
  assertthat::assert_that(is.data.frame(path))
  assertthat::assert_that(assertthat::has_name(path, c("lat", "lon", "stap_id")))
  if (nrow(path) == 0) {
    cli::cli_abort("{.var path} is empty.")
  }
  if (!all(path$stap_id %in% pressure$stap_id)) {
    cli::cli_warn("Some {.field stap_id} of {.var path} are not present in {.var tag$pressure}.")
  }

  # Remove pressure for stap not provided in path as well as flight
  # Identify flight which have no provided position in path for the stap before and after
  stap_id_interp <- pressure$stap_id
  id <- stap_id_interp == 0
  sequence <- seq_len(nrow(pressure))
  stap_id_interp[id] <- stats::approx(sequence[!id],
    stap_id_interp[!id], sequence[id],
    rule = 2
  )$y
  id <- ceiling(stap_id_interp) %in% path$stap_id[!is.na(path$lon)] &
    floor(stap_id_interp) %in% path$stap_id[!is.na(path$lon)]
  pressure_w_path <- pressure[id, ]

  # Create pressurepath by combining pressure and path
  pressurepath <- merge(
    pressure_w_path,
    path[!(names(path) %in% c("start", "end"))],
    by = "stap_id",
    all.x = TRUE
  )

  # Because merge change the order of pressure, we sort by date to keep the same original order
  pressurepath <- pressurepath[order(pressurepath$date), ]

  names(pressurepath)[names(pressurepath) == "value"] <- "pressure_tag"

  # Interpolate lat lon during flight
  id <- pressurepath$stap_id != round(pressurepath$stap_id)
  sequence <- seq_len(nrow(pressurepath))
  pressurepath$lat[id] <- stats::approx(sequence[!id],
    pressurepath$lat[!id], sequence[id],
    rule = 1
  )$y

  pressurepath$lon[id] <- stats::approx(sequence[!id],
    pressurepath$lon[!id], sequence[id],
    rule = 1
  )$y

  # Check workers
  assertthat::assert_that(is.numeric(workers) | workers == "auto")
  # GEE allows up to 100 requests at the same time, so we set the workers a little bit below
  if (workers == "auto") {
    workers <- max(1, min(90, round(nrow(pressurepath) / 1500)))
  }
  assertthat::assert_that(workers > 0 & workers < 100)


  # Format query
  body <- list(
    lon = pressurepath$lon,
    lat = pressurepath$lat,
    time = as.numeric(as.POSIXct(pressurepath$date)),
    variable = variable,
    dataset = era5_dataset,
    pressure = pressurepath$pressure_tag * 100,
    workers = workers
  )

  if (!quiet) {
    cli::cli_progress_step(
      "Generate request on {.url glp.mgravey.com/GeoPressure/v2/pressurePath} and download csv"
    )
  }

  if (debug) {
    temp_file <- tempfile("log_pressurepath_", fileext = ".json")
    write(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE), temp_file)
    cli::cli_text("Body request file: {.file {temp_file}}")
  }

  req <- httr2::request("https://glp.mgravey.com/GeoPressure/v2/pressurePath/") |>
    httr2::req_body_json(body, digit = 5, auto_unbox = FALSE)

  if (debug) {
    req <- httr2::req_verbose(req, body_req = TRUE, body_resp = TRUE, info = TRUE)
  }

  # Perform the request and convert the response to data.frame
  resp <- httr2::req_perform(req)
  resp_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)$data

  # If variable requested does not exist, the API return an empty list, which we
  # convert here as a NA
  out <- as.data.frame(lapply(resp_data, \(x) if (length(x) > 0) x else NA))

  # Check if the response is empty
  cols_with_na <- names(out)[vapply(out, function(x) any(is.na(x)), logical(1))]
  if (length(cols_with_na) > 0) {
    cli::cli_warn("The following columns contain `NA` values: {.val {cols_with_na}}")
  }

  if (!quiet) cli::cli_progress_step("Post-process pressurepath")

  # Convert time to date
  out$time <- as.POSIXct(out$time, origin = "1970-01-01", tz = "UTC")
  names(out)[names(out) == "time"] <- "date"

  # Add out to pressurepath
  pressurepath <- merge(
    pressurepath,
    out,
    all.x = TRUE
  )

  # Convert pressure Pa in hPa
  if ("surface_pressure" %in% names(pressurepath) && !all(is.na(pressurepath$surface_pressure))) {
    pressurepath$surface_pressure <- pressurepath$surface_pressure / 100

    # Compute surface_pressure_norm
    # Compute average pressure per stap_elev without including discard
    pp <- pressurepath
    pp$stapelev <- paste(pp$stap_id,
      ifelse(startsWith(pp$label, "elev_"), gsub("^.*?elev_", "", pp$label), "0"),
      sep = "|"
    )
    # trick to exclude discard from agg but still compute the norm value
    pp$stapelev_label <- pp$stapelev
    pp$stapelev_label[pp$label == "discard"] <- 0

    agg <- merge(
      stats::aggregate(surface_pressure ~ stapelev_label,
        data = pp,
        FUN = \(x) mean(x, na.rm = TRUE)
      ),
      stats::aggregate(pressure_tag ~ stapelev_label,
        data = pp,
        FUN = \(x) mean(x, na.rm = TRUE)
      )
    )
    id <- match(pp$stapelev, agg$stapelev)
    pressurepath$surface_pressure_norm <- pressurepath$surface_pressure -
      agg$surface_pressure[id] + agg$pressure_tag[id]
  }

  if (!is.null(solar_dep)) {
    # Add sunset and sunrise information
    twl <- path2twilight(pressurepath, solar_dep = solar_dep, return_long = FALSE)

    pressurepath <- merge(
      pressurepath,
      twl[, c("date", "sunset", "sunrise")]
    )
  }

  # Add additional parameter to pressurepath
  attr(pressurepath, "id") <- tag$param$id
  attr(pressurepath, "preprocess") <- preprocess
  attr(pressurepath, "sd") <- tag$param$geopressure_map$sd
  attr(pressurepath, "type") <- attr(path, "type")
  return(pressurepath)
}
