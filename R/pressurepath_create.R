#' Create a pressure path
#'
#' `pressurepath_create()` combines a tag pressure series with a path and retrieves matching ERA5
#' variables. Positions during flights are linearly interpolated between stationary periods.
#'
#' @section Data sources:
#' `source = "arco"` reads ECMWF's Analysis-Ready Cloud-Optimised (ARCO) archive directly. It is
#' optimised for surface pressure and altitude, requires an ECMWF API key and the optional `Rarr`
#' and `ecmwfr` packages, and supports only `"surface_pressure"` and `"altitude"`.
#'
#' `source = "api"` uses the hosted GeoPressureAPI. It needs no ECMWF key or `Rarr` installation
#' and can retrieve the wider set of ERA5 variables listed in `pressurepath_variable`.
#'
#' The default, `source = "auto"`, uses ARCO when a key stored by [ecmwfr::wf_set_key()] is
#' available and GeoPressureAPI otherwise. Use [pressurepath_create_arco()] or
#' [pressurepath_create_api()] to select a backend explicitly.
#'
#' @section Path and ERA5 processing:
#' Measurements are retained only when `path` contains the surrounding stationary periods.
#' Coordinates during flights are interpolated linearly, while ERA5 values are sampled at the
#' nearest grid cell and hour. `era5_dataset = "land"` uses 0.1 degree ERA5-Land,
#' `"single-levels"` uses 0.25 degree global ERA5, and `"both"` selects ERA5-Land over land and
#' global ERA5 over water.
#'
#' Surface pressure is returned in hPa and normalised to the mean tag pressure within each
#' stationary-period and elevation-label group. Observations labelled `"discard"` are excluded
#' from those means. Altitude is computed from tag pressure, ERA5 pressure and temperature, and
#' surface geopotential. If `solar_dep` is not `NULL`, local sunrise and sunset are added.
#'
#' @template ecmwf-key
#' @param tag A GeoPressureR `tag` object.
#' @param path A GeoPressureR `path` data.frame.
#' @param variable ERA5 variables to retrieve. ARCO supports `"altitude"` and
#'   `"surface_pressure"`; GeoPressureAPI supports additional variables.
#' @param solar_dep Solar depression angle used to compute sunrise and sunset, or `NULL` to skip
#'   this computation.
#' @param era5_dataset ERA5 product: `"land"`, `"single-levels"`, or `"both"` to use ERA5-Land
#'   over land and global ERA5 elsewhere.
#' @param preprocess Whether to preprocess pressure with [geopressure_map_preprocess()].
#' @param workers Number of parallel GeoPressureAPI requests, or `"auto"`.
#' @param source Data source: `"auto"`, `"arco"`, or `"api"`.
#' @param quiet Logical to suppress progress messages.
#' @param debug Logical to display request details.
#'
#' @return A `pressurepath` data.frame containing tag pressure, coordinates, requested ERA5
#'   variables, normalised surface pressure, and optional sunrise and sunset times.
#'
#' @examplesIf FALSE
#' pressurepath <- pressurepath_create(tag, path)
#'
#' @family pressurepath
#' @export
pressurepath_create <- function(
  tag,
  path = tag2path(tag),
  variable = c("altitude", "surface_pressure"),
  solar_dep = 0,
  era5_dataset = "both",
  preprocess = FALSE,
  workers = "auto",
  quiet = FALSE,
  debug = FALSE,
  source = c("auto", "arco", "api")
) {
  era5_dataset <- match.arg(era5_dataset, c("both", "land", "single-levels"))
  assertthat::assert_that(is.logical(quiet))
  source <- ecmwf_select_source(source, quiet)

  if (source == "arco") {
    unsupported <- setdiff(variable, c("altitude", "surface_pressure"))
    if (length(unsupported) > 0) {
      cli::cli_abort(c(
        "x" = "ARCO does not support variable{?s} {.val {unsupported}}.",
        "i" = "Use {.code source = \"api\"} for additional ERA5 variables."
      ))
    }
    arco_require_dependencies()
    pressurepath <- pressurepath_prepare(tag, path, preprocess, quiet)
    return(pressurepath_create_arco_impl(
      tag = tag,
      path = path,
      pressurepath = pressurepath,
      variable = variable,
      solar_dep = solar_dep,
      era5_dataset = era5_dataset,
      preprocess = preprocess,
      quiet = quiet,
      debug = debug
    ))
  }

  pressurepath <- pressurepath_prepare(tag, path, preprocess, quiet)
  pressurepath_create_api_impl(
    tag = tag,
    path = path,
    pressurepath = pressurepath,
    variable = variable,
    solar_dep = solar_dep,
    era5_dataset = era5_dataset,
    preprocess = preprocess,
    workers = workers,
    quiet = quiet,
    debug = debug
  )
}

pressurepath_prepare <- function(tag, path, preprocess, quiet) {
  tag_assert(tag, "stap")
  assertthat::assert_that(is.logical(preprocess))
  if (!quiet) {
    cli::cli_progress_step("Prepare pressure path")
  }
  pressure <- if (preprocess) {
    geopressure_map_preprocess(tag, compute_known = TRUE)
  } else {
    tag$pressure
  }
  assertthat::assert_that(nrow(pressure) > 0)
  assertthat::assert_that(is.data.frame(path))
  assertthat::assert_that(assertthat::has_name(path, c("lat", "lon", "stap_id")))
  if (nrow(path) == 0) {
    cli::cli_abort("{.var path} is empty.")
  }
  if (!all(path$stap_id %in% pressure$stap_id)) {
    cli::cli_warn("Some {.field stap_id} of {.var path} are not present in {.var tag$pressure}.")
  }

  stap_id_interp <- pressure$stap_id
  id <- stap_id_interp == 0
  sequence <- seq_len(nrow(pressure))
  stap_id_interp[id] <- stats::approx(
    sequence[!id],
    stap_id_interp[!id],
    sequence[id],
    rule = 2
  )$y
  id <- ceiling(stap_id_interp) %in%
    path$stap_id[!is.na(path$lon)] &
    floor(stap_id_interp) %in% path$stap_id[!is.na(path$lon)]
  pressurepath <- merge(
    pressure[id, ],
    path[, intersect(c("stap_id", "lat", "lon", "j"), names(path)), drop = FALSE],
    by = "stap_id",
    all.x = TRUE
  )
  pressurepath <- pressurepath[order(pressurepath$date), ]
  names(pressurepath)[names(pressurepath) == "value"] <- "pressure_tag"

  id <- pressurepath$stap_id != round(pressurepath$stap_id)
  sequence <- seq_len(nrow(pressurepath))
  pressurepath$lat[id] <- stats::approx(
    sequence[!id],
    pressurepath$lat[!id],
    sequence[id],
    rule = 1
  )$y
  pressurepath$lon[id] <- stats::approx(
    sequence[!id],
    pressurepath$lon[!id],
    sequence[id],
    rule = 1
  )$y
  pressurepath
}

pressurepath_finalize <- function(
  pressurepath,
  tag,
  path,
  preprocess,
  solar_dep,
  surface_pressure_pa = FALSE,
  date_first = FALSE
) {
  if ("surface_pressure" %in% names(pressurepath) && !all(is.na(pressurepath$surface_pressure))) {
    if (surface_pressure_pa) {
      pressurepath$surface_pressure <- pressurepath$surface_pressure / 100
    }
    pp <- pressurepath
    pp$stapelev <- paste(
      pp$stap_id,
      ifelse(startsWith(pp$label, "elev_"), gsub("^.*?elev_", "", pp$label), "0"),
      sep = "|"
    )
    pp$stapelev_label <- pp$stapelev
    pp$stapelev_label[pp$label == "discard"] <- 0
    agg <- merge(
      stats::aggregate(
        surface_pressure ~ stapelev_label,
        data = pp,
        FUN = \(x) mean(x, na.rm = TRUE)
      ),
      stats::aggregate(
        pressure_tag ~ stapelev_label,
        data = pp,
        FUN = \(x) mean(x, na.rm = TRUE)
      )
    )
    id <- match(pp$stapelev, agg$stapelev)
    pressurepath$surface_pressure_norm <- pressurepath$surface_pressure -
      agg$surface_pressure[id] +
      agg$pressure_tag[id]
  }

  if (!is.null(solar_dep)) {
    twl <- path2twilight(pressurepath, solar_dep = solar_dep, return_long = FALSE)
    pressurepath <- merge(pressurepath, twl[, c("date", "sunset", "sunrise")])
  }
  if (date_first) {
    pressurepath <- pressurepath[c("date", setdiff(names(pressurepath), "date"))]
  }
  attr(pressurepath, "id") <- tag$param$id
  attr(pressurepath, "preprocess") <- preprocess
  attr(pressurepath, "sd") <- tag$param$geopressure_map$sd
  attr(pressurepath, "type") <- attr(path, "type")
  pressurepath
}
