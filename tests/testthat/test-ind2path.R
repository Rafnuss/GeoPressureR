library(testthat)
library(GeoPressureR)

# set working directory to load the file
setwd(system.file("extdata/", package = "GeoPressureR"))

tag <- tag_read("18LX") |>
  tag_label()

geostap <- tag |>
  geostap_create(extent = c(-16, 23, 0, 50), scale = 1)

test_that("Check ind2path() output", {
  path_m <- matrix(c(1652, 1652, 1652, 1652, 1652, 1652, 1652, 1652, 1652, 1652, 1653, 1606, 1653, 1504, 1604, 1704, 1504, 1653, 1753, 1701, 1805, 1457, 1408, 1657, 1609, 1903, 1506, 1757, 1856, 1804, 1607, 1514, 1611, 1457, 1860, 1802, 1759, 1609, 1760, 1208, 1505, 1314, 1559, 1357, 1758, 1852, 1661, 1706, 1862, 1358), nrow = 10)
  path_c <- c(1652, 1603, 1755, 1708, 1607)

  expect_no_error(df <- ind2path(path_m, geostap))
  expect_true(nrow(df) == 50)

  expect_no_error(df <- ind2path(path_c, geostap))

  path_c_na <- path_c
  path_c_na[3] <- NA
  expect_no_error(df <- ind2path(path_c_na, geostap))
  expect_true(is.na(df$ind[3]))
})
