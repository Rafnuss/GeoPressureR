library(testthat)
library(GeoPressureR)

test_that("stap2flight() | default", {
  stap <- data.frame(
    start = seq(
      as.POSIXct("1990-01-01"),
      as.POSIXct("1991-01-01"),
      length.out = 13
    )
  )
  stap$end <- stap$start + 60 * 60 * 24 * 30
  stap$stap_id <- seq_len(nrow(stap))

  flight <- stap2flight(stap)
  expect_true(is.data.frame(flight))
  expect_true(nrow(flight) == nrow(stap) - 1)
  expect_true(all(flight$n == 1))

  flight <- stap2flight(stap, include_stap_id = c(1, 2, 4, 8, 13))
  expect_true(nrow(flight) == 4)
  expect_true(all(flight$n == c(1, 2, 4, 5)))

  flight <- stap2flight(stap, include_stap_id = c(3, 5))
  expect_true(nrow(flight) == 1)
  expect_true(flight$n == 2)

  flight <- stap2flight(stap, format = "list", units = "secs")
  expect_true(is.list(flight))
})
