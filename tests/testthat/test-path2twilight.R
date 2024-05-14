library(testthat)
library(GeoPressureR)

# created with dput(tag2path(tag)))
path <- data.frame(
  stap_id = c(1, 2, 3, 4, 5),
  j = c(1L, 1L, 1L, 1L, 1L),
  ind = c(1652L, 1603L, 1505L, 1609L, 1463L),
  lat = c(48.9, 47.5, 45.5, 41.5, 37.5),
  lon = c(17.05, 16.5, 14.5, 16.5, 13.5),
  start = as.POSIXct(c(1501113450, 1501888650, 1501987950, 1502075550, 1502151150), tzone = "UTC"),
  end = as.POSIXct(c(1501876050, 1501961250, 1502046750, 1502133150, 1502323050), tzone = "UTC"),
  include = c(TRUE, TRUE, TRUE, TRUE, TRUE),
  known = c(TRUE, FALSE, FALSE, FALSE, FALSE),
  interp = c(FALSE, FALSE, FALSE, FALSE, FALSE)
)

test_that("Check path2twilight() with path", {
  twl <- expect_no_error(path2twilight(path))
  expect_true(is.data.frame(twl))
  expect_true(all(c("twilight", "rise", "stap_id", "lon", "lat", "date") %in% names(twl)))

  date <- seq(as.POSIXct("2017-08-1 12:00:00"), as.POSIXct("2017-08-10 12:00:00"), by = "day")
  twl <- expect_no_error(path2twilight(path, date = date))
  expect_true(nrow(twl) == length(date) * 2)
})

# test_that("Check path2twilight() with pressurepath", {})
