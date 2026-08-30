#' Create a pressure path with GeoPressureAPI
#'
#' This is the explicit hosted API backend for [pressurepath_create()]. See the parent function for
#' the shared workflow, backend comparison, ECMWF key setup, and output details.
#'
#' @inheritParams pressurepath_create
#' @return See [pressurepath_create()].
#' @family pressurepath
#' @export
pressurepath_create_api <- function(
  tag,
  path = tag2path(tag),
  variable = c("altitude", "surface_pressure"),
  solar_dep = 0,
  era5_dataset = "both",
  preprocess = FALSE,
  workers = "auto",
  quiet = FALSE,
  debug = FALSE
) {
  pressurepath_create(
    tag = tag,
    path = path,
    variable = variable,
    solar_dep = solar_dep,
    era5_dataset = era5_dataset,
    preprocess = preprocess,
    workers = workers,
    source = "api",
    quiet = quiet,
    debug = debug
  )
}

pressurepath_create_api_impl <- function(
  tag,
  path = tag2path(tag),
  pressurepath,
  variable = c("altitude", "surface_pressure"),
  solar_dep = 0,
  era5_dataset = "both",
  preprocess = FALSE,
  workers = "auto",
  quiet = FALSE,
  debug = FALSE
) {
  era5_dataset <- match.arg(
    era5_dataset,
    choices = c("single-levels", "land", "both")
  )

  # Validate requested variables against the allowed set
  unknown_vars <- setdiff(variable, c(pressurepath_variable, "altitude"))
  assertthat::assert_that(
    length(unknown_vars) == 0,
    msg = paste0(
      "Unknown variable(s): ",
      paste(unknown_vars, collapse = ", "),
      ". Allowed variables are: ",
      paste(c(pressurepath_variable, "altitude"), collapse = ", ")
    )
  )

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

  req <- httr2::request(
    "https://glp.mgravey.com/GeoPressure/v2/pressurePath/"
  ) |>
    httr2::req_body_json(body, digit = 5, auto_unbox = FALSE)

  if (debug) {
    req <- httr2::req_verbose(
      req,
      body_req = TRUE,
      body_resp = TRUE,
      info = TRUE
    )
  }

  # Perform the request and convert the response to data.frame
  resp <- httr2::req_perform(req)
  resp_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)$data

  # If variable requested does not exist, the API return an empty list, which we
  # convert here as a NA
  out <- as.data.frame(lapply(resp_data, \(x) if (length(x) > 0) x else NA))

  # Check if the response is empty
  cols_with_na <- names(out)[vapply(out, function(x) anyNA(x), logical(1))]
  if (length(cols_with_na) > 0) {
    cli::cli_warn(
      "The following columns contain `NA` values: {.val {cols_with_na}}"
    )
  }

  if (!quiet) {
    cli::cli_progress_step("Post-process pressurepath")
  }

  # Convert time to date
  out$time <- as.POSIXct(out$time, origin = "1970-01-01", tz = "UTC")
  names(out)[names(out) == "time"] <- "date"

  # Add out to pressurepath
  pressurepath <- merge(
    pressurepath,
    out,
    all.x = TRUE
  )

  pressurepath_finalize(
    pressurepath,
    tag,
    path,
    preprocess,
    solar_dep,
    surface_pressure_pa = TRUE
  )
}
