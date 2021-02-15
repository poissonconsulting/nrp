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

