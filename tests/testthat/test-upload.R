context("upload")

test_that("nrp_upload works", {

  path <-  system.file("extdata", "ctd/2018/KL1_27Aug2018008downcast.cnv",
                       package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd_file(path = path)
  file.copy("inst/extdata/database_template/test-nrp.sqlite",
            "inst/extdata/upload")
  db_path <- "inst/extdata/upload/test-nrp.sqlite"
  nrp_upload(data = data, db_path = db_path, commit = TRUE)
  conn <- readwritesqlite::rws_open_connection("inst/extdata/upload/test-nrp.sqlite")
  db_data <- readwritesqlite::rws_read_sqlite_table("CTD", conn = conn)
  readwritesqlite::rws_close_connection(conn = conn)

  expect_equal(data, db_data)
  expect_identical(length(db_data), 13L)
  expect_identical(nrow(db_data), 1445L)

  unlink("inst/extdata/upload/test-nrp.sqlite")


  path <-  system.file("extdata", "ctd/2018/KL1_27Aug2018008downcast.cnv",
                       package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd_file(path = path)
  file.copy("inst/extdata/database_template/test-nrp.sqlite",
            "inst/extdata/upload")
  db_path <- "inst/extdata/upload/test-nrp.sqlite"

  nrp_upload(data = data, db_path = db_path, commit = FALSE)

  conn <- readwritesqlite::rws_open_connection("inst/extdata/upload/test-nrp.sqlite")
  db_data <- readwritesqlite::rws_read_sqlite_table("CTD", conn = conn)
  readwritesqlite::rws_close_connection(conn = conn)

  expect_identical(length(db_data), 13L)
  expect_identical(nrow(db_data), 0L)

  unlink("inst/extdata/upload/test-nrp.sqlite")

})


