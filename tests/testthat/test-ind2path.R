library(testthat)
library(GeoPressureR)

# set working directory to load the file
test_with_extdata()

tag <- tag_create("18LX", quiet = TRUE) |>
  tag_label(quiet = TRUE) |>
  tag_set_map(extent = c(-16, 23, 0, 50), scale = 1)

test_that("Check ind2path() output", {
  ind_m <- matrix(
    c(
      1652,
      1652,
      1652,
      1652,
      1652,
      1652,
      1652,
      1652,
      1652,
      1652,
      1653,
      1606,
      1653,
      1504,
      1604,
      1704,
      1504,
      1653,
      1753,
      1701,
      1805,
      1457,
      1408,
      1657,
      1609,
      1903,
      1506,
      1757,
      1856,
      1804,
      1607,
      1514,
      1611,
      1457,
      1860,
      1802,
      1759,
      1609,
      1760,
      1208,
      1505,
      1314,
      1559,
      1357,
      1758,
      1852,
      1661,
      1706,
      1862,
      1358
    ),
    nrow = 10
  )
  ind_c <- c(1652, 1603, 1755, 1708, 1607)

  expect_no_error({
    path_m <- ind2path(ind_m, tag)
  })
  expect_true(nrow(path_m) == 50)

  expect_no_error({
    path_c <- ind2path(ind_c, tag)
  })

  ind_c_na <- ind_c
  ind_c_na[3] <- NA
  expect_no_error({
    path_c_na <- ind2path(ind_c_na, tag)
  })
  expect_true(is.na(path_c_na$ind[3]))
})
