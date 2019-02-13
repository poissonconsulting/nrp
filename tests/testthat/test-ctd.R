context("ctd")

test_that("nrp_read_ctd_file works", {
  path <-  system.file("extdata", "ctd/2018/KL1_27Aug2018008downcast.cnv",
                       package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd_file(path)
  expect_is(data, "tbl_df")
  expect_identical(attr(data, "path"), path)
  expect_is(attr(data, "flob"), "flob")
  expect_identical(check_ctd_data(data), data)
})

test_that("nrp_read_ctds works", {

  path <-  system.file("extdata", "ctd/2018", package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd(path)
  expect_is(data, "tbl_df")
  expect_identical(length(data), 13L)

  expect_identical(check_ctd_data(data), data)


  path <-  system.file("extdata", "ctd/2018", package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd(path, recursive = TRUE)
  expect_is(data, "tbl_df")
  expect_identical(length(data), 13L)

  expect_identical(check_ctd_data(data), data)


  path <-  system.file("extdata", "ctd", package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd(path)
  expect_identical(data,  list(x = 1)[-1])

})


test_that("nrp_load_ctd_sites", {

  site <- nrp_load_ctd_sites()

  expect_is(site, "tbl_df")
  expect_identical(length(site), 6L)
  expect_identical(nrow(site), 17L)

})




