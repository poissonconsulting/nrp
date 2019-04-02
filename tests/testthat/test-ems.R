context("ems")

test_that("nrp_extract_ems works", {
  conn <- nrp_create_db(path = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))
  path <-  system.file("extdata", "ems/test_ems.rds", package = "nrp", mustWork = TRUE)
  ems <- readRDS(path)
  data <- nrp_extract_ems(data = ems, db_path = conn, analysis_type = "metals")


  expect_is(data, "tbl_df")
  check_ems_metals_data(data, exclusive = TRUE, order = TRUE)
  expect_identical(nrow(data), 24L)
  expect_identical(length(data), 148L)

  data <- nrp_extract_ems(data = ems, db_path = conn, analysis_type = "standard")

  expect_is(data, "tbl_df")
  check_ems_standard_data(data, exclusive = TRUE, order = TRUE)
  expect_identical(nrow(data), 172L)
  expect_identical(length(data), 38L)

})

test_that("nrp_upload_ems works", {

  conn <- nrp_create_db(path = ":memory:", ask = FALSE)
  path <-  system.file("extdata", "ems/test_ems.rds", package = "nrp", mustWork = TRUE)
  ems <- readRDS(path)
  data <- nrp_extract_ems(data = ems, db_path = conn, analysis_type = "metals")

  nrp_upload_ems_metals(data = data, db_path = conn)
  db_data <- readwritesqlite::rws_read_table("metalsEMS", conn = conn)

  expect_identical(nrow(db_data), 24L)
  expect_identical(length(db_data), 148L)
  readwritesqlite::rws_disconnect(conn)

  conn <- nrp_create_db(path = ":memory:", ask = FALSE)
  data <- nrp_extract_ems(data = ems, db_path = conn, analysis_type = "standard")

  nrp_upload_ems_standard(data = data, db_path = conn)
  db_data <- readwritesqlite::rws_read_table("standardEMS", conn = conn)
  expect_error(nrp_upload_ems_standard(data = data, db_path = conn))

  readwritesqlite::rws_disconnect(conn)

  expect_identical(nrow(db_data), 172L)
  expect_identical(length(db_data), 38L)

})

test_that("nrp_download_ems works", {

  conn <- nrp_create_db(path = ":memory:", ask = FALSE)
  path <-  system.file("extdata", "ems/test_ems.rds", package = "nrp", mustWork = TRUE)
  ems <- readRDS(path)
  data <- nrp_extract_ems(data = ems, db_path = conn, analysis_type = "metals")
  nrp_upload_ems_metals(data = data, db_path = conn)

  data <- nrp_extract_ems(data = ems, db_path = conn, analysis_type = "standard")
  nrp_upload_ems_standard(data = data, db_path = conn)


  db_data_metals_no_det_lims <- nrp_download_ems(start_date_time = "2018-08-27 11:50:00",
                              end_date_time = "2018-09-03 18:38:00",
                              sites = NULL,
                              db_path = conn,
                              analysis_type = "metals",
                              show_detection_limits = FALSE)

  expect_is(db_data_metals_no_det_lims, "tbl_df")
  expect_identical(length(db_data_metals_no_det_lims), 78L)
  expect_identical(nrow(db_data_metals_no_det_lims), 14L)

  db_data_metals <- nrp_download_ems(start_date_time = "2018-08-27 11:50:00",
                                     end_date_time = "2018-09-03 18:38:00",
                                     sites = NULL,
                                     db_path = conn,
                                     analysis_type = "metals",
                                     show_detection_limits = TRUE)

  check_ems_metals_data(db_data_metals, exclusive = TRUE, order = TRUE)

  db_data_standard <- nrp_download_ems(start_date_time = "2018-07-03 13:48:00",
                                     end_date_time = "2018-07-31 13:28:00",
                                     sites = NULL,
                                     db_path = conn,
                                     analysis_type = "standard",
                                     show_detection_limits = TRUE)

  check_ems_standard_data(db_data_standard, exclusive = TRUE, order = TRUE)
  expect_is(db_data_standard, "tbl_df")
  expect_identical(length(db_data_standard), 38L)
  expect_identical(nrow(db_data_standard), 48L)
})

