library(testthat)
library(GeoPressureR)

# Small synthetic case
tag_sm <- list()
tag_sm$pressure <- data.frame(
  date = as.POSIXct(c(
    "2017-06-20 00:00:00 UTC", "2017-06-20 01:00:00 UTC",
    "2017-06-20 02:00:00 UTC", "2017-06-20 03:00:00 UTC"
  ), tz = "UTC"),
  value = c(1000, 1000, 1000, 1000),
  label = c("", "", "", "")
)

mismatch <- geopressure_mismatch(tag_sm, extent = c(0, 1, 0, 1), scale = 1)
likelihood <- geopressure_likelihood(mismatch)

test_that("geopressure_mismatch() | default output", {
  expect_type(mismatch, "list")
  expect_length(mismatch, 1)
  expect_type(mismatch[[1]], "list")
  expect_true(all(c("mse", "mask", "stap", "nb_sample", "start", "end") %in% names(mismatch[[1]])))
  expect_equal(length(dim(mismatch[[1]]$mse)), 2)
  expect_equal(length(dim(mismatch[[1]]$mask)), 2)
  expect_true(mismatch[[1]]$stap == 1)
  expect_true(mismatch[[1]]$nb_sample == 4)
  expect_true(mismatch[[1]]$start == tag_sm$pressure$date[1])
  expect_true(mismatch[[1]]$end == tag_sm$pressure$date[4])
})

test_that("geopressure_mismatch() | incorrect pressure", {
  tag_sm_2 <- tag_sm
  tag_sm_2$pressure$value[2] <- 1200
  expect_error(
    geopressure_mismatch(tag_sm_2, extent = c(0, 1, 0, 1), scale = 1),
    "*Pressure observation should be between 250 hPa*"
  )
})

test_that("geopressure_mismatch() | warning for irregular date", {
  tag_sm_2 <- tag_sm
  tag_sm_2$pressure$date[3] <- tag_sm_2$pressure$date[3] + 1
  expect_warning(
    geopressure_mismatch(tag_sm_2, extent = c(0, 1, 0, 1), scale = 1),
    "*The pressure data is not on a regular interval*"
  )
})

test_that("geopressure_mismatch() | date too recent", {
  tag_sm_2 <- tag_sm
  tmp <- Sys.time()
  tag_sm_2$pressure$date <- tmp + c(0, 1, 2, 3) * 60 * 60
  expect_error(
    geopressure_mismatch(tag_sm_2, extent = c(0, 1, 0, 1), scale = 1),
    "*made for periods where no data are available*"
  )
})

test_that("geopressure_mismatch() | timeout and worker", {
  expect_error(geopressure_mismatch(tag, extent = c(0, 1, 0, 1), scale = 1, timeout = 1))
  expect_error(geopressure_mismatch(tag_sm, extent = c(0, 1, 0, 1), scale = 1, worker = 100))
})

test_that("geopressure_likelihood() | default output", {
  expect_type(likelihood, "list")
  expect_length(likelihood, 1)
  expect_type(likelihood[[1]], "list")
  expect_true(all(c("likelihood", "stap", "start", "end") %in% names(likelihood[[1]])))
  expect_equal(length(dim(likelihood[[1]]$likelihood)), 2)

  expect_error(geopressure_likelihood(mismatch, s = "not_a_number"))
  expect_error(geopressure_likelihood(mismatch, thr = "not_a_number"))
  expect_error(geopressure_likelihood(mismatch, fun_w = "not_a_function"))
})








# Start by computing all the necessary file for the tests
tag <- tag_read(
  directory = system.file("extdata/0_tag/18LX", package = "GeoPressureR"),
  crop_start = "2017-08-01", crop_end = "2017-10-01"
)
tag <- trainset_read(
  tag,
  directory = system.file("extdata/1_pressure/labels", package = "GeoPressureR")
)

test_that("geopressure_map | full example", {
  expect_no_error(pressure_mismatch <- geopressure_mismatch(tag,
    extent = c(-16, 23, 0, 50),
    scale = 1,
    max_sample = 100,
    margin = 30
  ))

  expect_no_error(pressure_likelihood <- geopressure_likelihood(pressure_mismatch))
})

test_that("geopressure_map | Incomplete stap", {
  tag_tmp <- tag_stap(tag)
  tag_tmp$pressure <- subset(tag_tmp$pressure, stap == 3 | stap == 4)

  # Warning because of iregular interval
  expect_warning(mismatch <- geopressure_mismatch(tag_tmp,
    extent = c(-16, 23, 0, 50),
    scale = 1,
  ), "*data is not on a regular interval*")
  expect_true(length(mismatch) == nrow(tag_tmp$stap))
  expect_equal(length(dim(mismatch[[3]]$mse)), 2)

  # Check geopressure_likelihood
  likelihood <- geopressure_likelihood(mismatch)
  expect_true(length(likelihood) == nrow(tag_tmp$stap))
  expect_equal(length(dim(likelihood[[4]]$likelihood)), 2)

  # Check map2path
  expect_no_error(map2path(likelihood))
  expect_no_error(map2path(likelihood, interp = 0.2))
})


test_that("geopressure_map | with elev_", {
  tag_tmp <- trainset_read(tag,
    directory = system.file("extdata/1_pressure/labels/", package = "GeoPressureR"),
    filename = "18LX_act_pres-labeled-elev.csv"
  )
  tag_tmp <- tag_stap(tag_tmp)
  tag_tmp$pressure <- subset(tag_tmp$pressure, stap == 1)

  mismatch <- geopressure_mismatch(tag_tmp,
    extent = c(-16, 23, 0, 50),
    scale = 1,
  )
  expect_length(mismatch, 5)
  expect_equal(length(dim(mismatch[[1]]$mse)), 2)
})
