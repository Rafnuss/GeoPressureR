library(testthat)
library(GeoPressureR)


# Manual test
if (FALSE) {
  n <- 1
  ws <- runif(n, min = -1, max = 1) + runif(n, min = -1, max = 1) * 1i
  gs <- runif(n, min = -1, max = 1) + runif(n, min = -1, max = 1) * 1i

  wp <- windsupport(ws, gs)

  wp2 <- ws * Conj(gs) / gs * Conj(gs)

  speed <- c(ws, gs, wp)
  plot(Re(speed), Im(speed), xlim = c(-1, 1), ylim = c(-1, 1))
  grid()
}

test_that("windsupport()", {
  expect_equal(windsupport(1i, 1i), 1)
  expect_equal(windsupport(1i, -1i), -1)
  expect_equal(windsupport(1i, 1 + 1i), sqrt(2) / 2)
})
