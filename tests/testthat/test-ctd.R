context("ctd")

test_that("nrp_read_ctd works", {
  path <-  system.file("extdata", "ctd/2018/KL1_27Aug2018008downcast.cnv",
                       package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd(path)
  expect_is(data, "tbl_df")
})
