library(testthat)
library(GeoPressureR)

# Set working directory
setwd(system.file("extdata", package = "GeoPressureR"))

# Helper to create mock tag
create_mock_tag <- function() {
  tag <- tag_create("18LX", quiet = TRUE) |>
    tag_label(quiet = TRUE) |>
    tag_set_map(extent = c(-16, 23, 0, 50), scale = 1)

  # Add pressure data
  tag$pressure <- data.frame(
    date = seq(
      as.POSIXct("2017-06-20 00:00:00", tz = "UTC"),
      as.POSIXct("2017-06-25 00:00:00", tz = "UTC"),
      by = "hour"
    ),
    value = runif(121, 950, 1013)
  )

  # Add stap data with required columns
  tag$stap <- data.frame(
    stap_id = 1:2,
    start = as.POSIXct(c("2017-06-20 00:00:00", "2017-06-23 00:00:00"), tz = "UTC"),
    end = as.POSIXct(c("2017-06-21 00:00:00", "2017-06-24 00:00:00"), tz = "UTC"),
    known_lat = c(NA, NA),
    known_lon = c(NA, NA),
    include = c(TRUE, TRUE)
  )

  # Ensure param$tag_set_map exists
  if (is.null(tag$param$tag_set_map)) {
    tag$param$tag_set_map <- list(extent = c(-16, 23, 0, 50), scale = 1)
  }

  tag
}

test_that("tag_download_wind() | input validation", {
  tag <- create_mock_tag()

  # No map
  expect_error(tag_download_wind(tag_create("18LX", quiet = TRUE)), "tag_set_map")

  # Mock API for subsequent checks
  local_mocked_bindings(
    wf_request_batch = function(...) list(code = 200, message = "Success"),
    .package = "ecmwfr"
  )

  # Invalid parameters
  tmp <- withr::local_tempdir()
  file_func <- function(s, t) file.path(tmp, paste0(t, "_", s, ".nc"))

  expect_error(tag_download_wind(tag, extent = 1:3), "length.*extent.*4")
  expect_error(tag_download_wind(tag, file = "not_func"), "file.*function")
  expect_error(tag_download_wind(tag, variable = 123, file = file_func), "variable.*character")
  # Suppress directory creation warning that happens before validation
  suppressWarnings(
    expect_error(
      tag_download_wind(tag, include_stap_id = 99, file = file_func),
      "include_stap_id.*stap"
    )
  )
})

test_that("tag_download_wind() | file handling and warnings", {
  tag <- create_mock_tag()
  tmp <- withr::local_tempdir()

  local_mocked_bindings(
    wf_request_batch = function(...) list(code = 200, message = "Success"),
    wf_set_key = function(...) invisible(NULL),
    .package = "ecmwfr"
  )

  # Directory creation warning
  file_func <- function(s, t) file.path(tmp, "subdir", paste0(t, "_", s, ".nc"))
  expect_warning(
    tag_download_wind(tag, include_stap_id = 1, file = file_func),
    "did not exist"
  )
  expect_true(dir.exists(file.path(tmp, "subdir")))

  # Last stap warning
  expect_warning(
    tag_download_wind(tag, include_stap_id = c(1, 2), file = file_func),
    "included the last stationary period"
  )

  # Deprecated token warning
  expect_warning(
    tag_download_wind(tag, include_stap_id = 1, cds_token = "x", file = file_func),
    "deprecated"
  )

  # Existing file error
  dir.create(file.path(tmp, "exist"))
  f_exist <- file.path(tmp, "exist", "18LX_1.nc")
  file.create(f_exist)
  expect_error(
    tag_download_wind(
      tag,
      include_stap_id = 1,
      file = function(s, t) file.path(tmp, "exist", paste0(t, "_", s, ".nc")),
      overwrite = FALSE
    ),
    "already ERA5 data file"
  )

  # Skip existing file (no error)
  expect_no_error(
    tag_download_wind(
      tag,
      include_stap_id = NULL,
      file = function(s, t) file.path(tmp, "exist", paste0(t, "_", s, ".nc")),
      overwrite = FALSE
    )
  )
})

test_that("tag_download_wind() | request generation", {
  tag <- create_mock_tag()
  # Set specific pressure to test level selection
  tag$pressure$value <- rep(c(1000, 500, 200), length.out = nrow(tag$pressure))

  captured <- new.env()

  local_mocked_bindings(
    wf_request_batch = function(request_list, workers, ...) {
      captured$request <- request_list
      captured$workers <- workers
      list(code = 200, message = "Success")
    },
    .package = "ecmwfr"
  )

  tmp <- withr::local_tempdir()

  tag_download_wind(
    tag,
    include_stap_id = 1,
    variable = c("var1", "var2"),
    workers = 5,
    file = function(s, t) file.path(tmp, paste0(t, "_", s, ".nc"))
  )

  # Check workers
  expect_equal(captured$workers, 5)

  # Check request structure
  req <- captured$request[[1]]
  expect_type(req, "list")
  expect_equal(req$dataset_short_name, "reanalysis-era5-pressure-levels")
  expect_equal(req$product_type, "reanalysis")
  expect_equal(req$data_format, "netcdf")
  expect_equal(req$area, c(50, -16, 0, 23)) # N, W, S, E
  expect_equal(req$target, "18LX_1.nc")

  # Check variables
  expect_equal(req$variable, c("var1", "var2"))

  # Check pressure levels (should cover 200-1000)
  expect_true(min(req$pressure_level) <= 200)
  expect_true(max(req$pressure_level) >= 1000)

  # Check time
  expect_true(all(c("year", "month", "day", "time") %in% names(req)))
  expect_gt(length(req$time), 0)
})
