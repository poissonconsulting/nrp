context("ems")

test_that("nrp_extract_ems works", {
  # conn <- nrp_create_db(path = ":memory:", ask = FALSE)
  # teardown(DBI::dbDisconnect(conn))
  path <-  system.file("extdata", "ems/ems.rds", package = "nrp", mustWork = TRUE)
  ems <- readRDS(path)
  data <- nrp_extract_ems(data = ems, analysis_type = "metals")

  expect_is(data, "tbl_df")
  expect_identical(nrow(data), 58L)
  expect_identical(length(data), 147L)

})
