library(testthat)
library(GeoPressureR)

# Manual test
if (FALSE) {
  n <- 1
  speed <- runif(n, min = -1, max = 1) + runif(n, min = -1, max = 1) * 1i
  plot(Re(speed), Im(speed), xlim = c(-1, 1), ylim = c(-1, 1))
  grid()

  speed_to_bearing(speed)
}

test_that("speed2bearing()", {
  speed <- c(1, 0, -1, 0) + c(0, 1, 0, -1) * 1i # E, N, W, S
  expect_equal(speed2bearing(speed), c(90, 0, 270, 180))
  expect_equal(speed2bearing(speed, speed_ref = 1), c(0, 270, 180, 90))
})
