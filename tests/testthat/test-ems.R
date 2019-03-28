context("ems")

test_that("nrp_extract_ems works", {
  conn <- nrp_create_db(path = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))
  path <-  system.file("extdata", "ems/test_ems.rds", package = "nrp", mustWork = TRUE)
  ems <- readRDS(path)
  data <- nrp_extract_ems(data = ems, db_path = conn, analysis_type = "metals")

  expect_is(data, "tbl_df")
  expect_identical(nrow(data), 31L)
  expect_identical(length(data), 148L)

  data <- nrp_extract_ems(data = ems, db_path = conn, analysis_type = "standard")
  expect_identical(nrow(data), 179L)
  expect_identical(length(data), 38L)

})

test_that("nrp_upload_ctd works", {

  conn <- nrp_create_db(path = ":memory:", ask = FALSE)
  path <-  system.file("extdata", "ems/test_ems.rds", package = "nrp", mustWork = TRUE)
  ems <- readRDS(path)
  data <- nrp_extract_ems(data = ems, db_path = conn, analysis_type = "metals")

  nrp_upload_ems_metals(data = data, db_path = conn)
  db_data <- readwritesqlite::rws_read_table("metalsEMS", conn = conn)

  expect_identical(nrow(data), 31L)
  expect_identical(length(data), 148L)
  readwritesqlite::rws_disconnect(conn)


  conn <- nrp_create_db(path = ":memory:", ask = FALSE)
  data <- nrp_extract_ems(data = ems, db_path = conn, analysis_type = "standard")

  nrp_upload_ems_standard(data = data, db_path = conn)
  db_data <- readwritesqlite::rws_read_table("standardEMS", conn = conn)
  readwritesqlite::rws_disconnect(conn)

  expect_identical(nrow(data), 179L)
  expect_identical(length(data), 38L)

})
