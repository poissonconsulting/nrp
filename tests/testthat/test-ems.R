context("ems")

test_that("nrp_extract_ems works", {
  # conn <- nrp_create_db(path = ":memory:", ask = FALSE)
  # teardown(DBI::dbDisconnect(conn))
  path <-  system.file("extdata", "ems/test_ems.rds", package = "nrp", mustWork = TRUE)
  ems <- readRDS(path)
  data <- nrp_extract_ems(data = ems, analysis_type = "metals")

  expect_is(data, "tbl_df")
  expect_identical(nrow(data), 31L)
  expect_identical(length(data), 147L)

  data <- nrp_extract_ems(data = ems, analysis_type = "standard")
  expect_identical(nrow(data), 179L)
  expect_identical(length(data), 37L)


})
