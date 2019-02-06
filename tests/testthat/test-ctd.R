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
  path <-  system.file("extdata", "ctd", package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd(path)
  expect_identical(data,  list(x = 1)[-1])

  path <-  system.file("extdata", "ctd/2018", package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd(path)
  expect_is(data, "list")
  expect_identical(length(data), 1L)
  expect_match(names(data), path)

  expect_is(data[[1]], "tbl_df")
  expect_match(attr(data[[1]], "path"), path)
  expect_is(attr(data[[1]], "flob"), "flob")

  data <- nrp_read_ctd(path, recursive = TRUE)
  expect_is(data, "list")
  expect_identical(length(data), 3L)
  expect_match(names(data), path)

  expect_is(data[[2]], "tbl_df")
  expect_match(attr(data[[2]], "path"), path)
  expect_is(attr(data[[2]], "flob"), "flob")
})
