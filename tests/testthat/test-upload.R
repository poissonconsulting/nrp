context("upload")

test_that("nrp_upload_data works", {

  path <-  system.file("extdata", "ctd/2018/KL1_27Aug2018008downcast.cnv",
                       package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd_file(path = path)

  conn <- nrp_create_db(path  = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))

  nrp_upload_data(data = data, conn = conn, commit = TRUE, strict = TRUE, silent = TRUE)

  db_data <- readwritesqlite::rws_read_sqlite_table("CTD", conn = conn)

  expect_equal(data, db_data)
  expect_identical(length(db_data), 14L)
  expect_identical(nrow(db_data), 1313L)

})

test_that("nrp_upload works", {

  path <-  system.file("extdata", "ctd/2018/KL1_27Aug2018008downcast.cnv",
                       package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd_file(path = path)
  conn <- nrp_create_db(path  = ":memory:", ask = FALSE)

  nrp_upload(data = data, conn = conn)

  db_data <- readwritesqlite::rws_read_sqlite_table("CTD", conn = conn)
  readwritesqlite::rws_close_connection(conn = conn)

  expect_equal(data, db_data)
  expect_identical(length(db_data), 14L)
  expect_identical(nrow(db_data), 1313L)


  conn <- nrp_create_db(path  = ":memory:", ask = FALSE)

  expect_error(nrp_upload(data = data, conn = "wrong_path.sqlite"),
               "path 'wrong_path.sqlite' must exist")
  readwritesqlite::rws_close_connection(conn = conn)

  conn <- nrp_create_db(path  = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))

  wrong_file <- path
  expect_error(nrp_upload(data = data, conn = wrong_file),
               "File provided is not an SQLite database")


})



