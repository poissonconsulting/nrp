context("assess")


test_that("assess_data_type works", {

  path <-  system.file("extdata", "ctd/2018/KL1_27Aug2018008downcast.cnv",
                       package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd_file(path)
  data_type <- assess_data_type(data)
  expect_is(data_type, "character")
  expect_identical(data_type, "CTD")


  path <-  system.file("extdata", "ctd/2018/KL1_27Aug2018008downcast.cnv",
                       package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd_file(path) %>%
    select(-1)
  expect_error(assess_data_type(data),
               "The data you are attempting to upload does not match any known NRP data types")
})
