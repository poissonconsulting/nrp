
test_that("nrp_upload_ctd works", {

  conn <- nrp_create_db(path  = ":memory:", ask = FALSE)

  path <-  system.file("extdata", "ctd/2018/KL1_27Aug2018008downcast.cnv",
                       package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd_file(path = path, db_path = conn)


  nrp_upload_ctd(data = data, db_path = conn)

  db_data <- readwritesqlite::rws_read_sqlite_table("CTD", conn = conn)
  readwritesqlite::rws_close_connection(conn = conn)

  expect_equal(data, db_data)
  expect_identical(length(db_data), 15L)
  expect_identical(nrow(db_data), 1313L)


  conn <- nrp_create_db(path  = ":memory:", ask = FALSE)

  expect_error(nrp_upload_ctd(data = data, db_path = "wrong_path.sqlite"),
               "path 'wrong_path.sqlite' must exist")
  readwritesqlite::rws_close_connection(conn = conn)

  conn <- nrp_create_db(path  = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))

  wrong_file <- path
  expect_error(nrp_upload_ctd(data = data, db_path = wrong_file),
               "File provided is not an SQLite database")

})



