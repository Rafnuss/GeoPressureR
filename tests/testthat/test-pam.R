
test_that("Check pam_read()", {
  expect_type(pam_read(pathname = system.file("extdata", package = "GeoPressureR")), "list")
  expect_type(pam_read(
    pathname = system.file("extdata", package = "GeoPressureR"),
    crop_start = "2017-06-20", crop_end = "2018-05-02"
  ), "list")
  expect_true(nrow(pam_read(
    pathname = system.file("extdata", package = "GeoPressureR"),
    crop_start = "2019-06-20", crop_end = "2018-05-02"
  )$light) == 0)
})

test_that("Check trainset_read()", {
  pam_data <- pam_read(
    pathname = system.file("extdata", package = "GeoPressureR"),
    crop_start = "2017-06-20", crop_end = "2018-05-02"
  )
  expect_type(trainset_read(pam_data, pathname = system.file("extdata", package = "GeoPressureR")), "list")
})


test_that("Check pam_sta()", {
  pam_data <- pam_read(
    pathname = system.file("extdata", package = "GeoPressureR"),
    crop_start = "2017-06-20", crop_end = "2018-05-02"
  )
  pam_data <- trainset_read(pam_data, pathname = system.file("extdata", package = "GeoPressureR"))

  expect_error(pam_sta(pam_data), NA)
  expect_true(nrow(pam_sta(pam_data)$sta) > 0)
})
