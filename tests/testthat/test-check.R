context("check")

test_that("check_file_exist works", {
  path <- system.file("extdata", "ctd/2018/KL1_27Aug2018008downcast.cnv",
              package = "nrp", mustWork = TRUE)
  expect_identical(check_file_exists(path), path)
  path <- paste0(path, "rubbish", collapse = "")
  expect_error(check_file_exists(path),
               "file '.*ctd/2018/KL1_27Aug2018008downcast.cnvrubbish' must exist")
})
