test_that("check_file_exists works", {
  path <- system.file("extdata", "ctd/2018/KL1_27Aug2018008downcast.cnv",
              package = "nrp", mustWork = TRUE)
  expect_identical(check_file_exists(path), path)

  path <- system.file("extdata", "ctd",
              package = "nrp", mustWork = TRUE)
  expect_error(check_file_exists(path), "path '.*/extdata/ctd' must be a file")

  path <- paste0(path, "/missing", collapse = "")
  expect_error(check_file_exists(path),
               "path '.*/extdata/ctd/missing' must exist")
})

test_that("check_dir_exists works", {
  path <- system.file("extdata", "ctd/2018/KL1_27Aug2018008downcast.cnv",
              package = "nrp", mustWork = TRUE)
  expect_error(check_dir_exists(path),
               "path '.*/extdata/ctd/2018/KL1_27Aug2018008downcast.cnv' must be a directory")

  path <- system.file("extdata", "ctd",
              package = "nrp", mustWork = TRUE)
  expect_identical(check_dir_exists(path), path)

  path <- paste0(path, "/missing", collapse = "")
  expect_error(check_dir_exists(path),
               "path '.*/extdata/ctd/missing' must exist")
})

test_that("check_chr_date works", {
  x <- 1
  expect_error(check_chr_date(x), "`x` must be character.")

  x <- c("a", "a")
  expect_error(check_chr_date(x), "`x` must be a scalar (length 1).", fixed = TRUE)

  x <- "a"
  expect_error(check_chr_date(x), "Invalid date for `x`. Please use format: yyyy-mm-dd.")

})

test_that("check_chr_datetime works", {
  x <- 1
  expect_error(check_chr_datetime(x), "`x` must be character.")

  x <- c("a", "a")
  expect_error(check_chr_datetime(x), "`x` must be a scalar (length 1).", fixed = TRUE)

  x <- "a"
  expect_error(check_chr_datetime(x), "Invalid date-time for `x`. Please use format: yyyy-mm-dd hh:mm:ss with 24 hour time.")

  x <- "1/2/3"
  expect_error(check_chr_datetime(x), "Invalid date-time for `x`. Please use format: yyyy-mm-dd hh:mm:ss with 24 hour time.")

})
