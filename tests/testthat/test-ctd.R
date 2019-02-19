context("ctd")

test_that("nrp_read_ctd_file works", {

  path <-  system.file("extdata", "ctd/2018/KL1_27Aug2018008downcast.cnv",
                       package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd_file(path)
  expect_is(data, "tbl_df")
  expect_identical(check_ctd_data(data), data)

  path <-  system.file("extdata", "bad ctd/2018/KL_Badly_named_file_1.cnv",
                       package = "nrp", mustWork = TRUE)
  expect_error(data <- nrp_read_ctd_file(path),
               "Station name could not be extracted from file name: No matches")

  path <-  system.file("extdata", "bad ctd/2018/KL1_AR1_Badly_named_file_2.cnv",
                       package = "nrp", mustWork = TRUE)
  expect_error(data <- nrp_read_ctd_file(path),
               "Station name could not be extracted from file name: More than one match")

})

test_that("nrp_read_ctd works", {

  path <-  system.file("extdata", "ctd/2018", package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd(path)
  expect_is(data, "tbl_df")
  expect_identical(length(data), 14L)

  expect_identical(check_ctd_data(data), data)


  path <-  system.file("extdata", "ctd/2018", package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd(path, recursive = TRUE)
  expect_is(data, "tbl_df")
  expect_identical(length(data), 14L)

  expect_identical(check_ctd_data(data), data)


  path <-  system.file("extdata", "ctd", package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd(path)
  expect_identical(data,  list(x = 1)[-1])

})


test_that("nrp_load_ctd works", {

  path <-  system.file("extdata", "ctd/2018", package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd(path = path, recursive = TRUE)

  conn <- readwritesqlite::rws_open_connection("")
  readwritesqlite::rws_write_sqlite(data[0, ], exists = F, conn = conn, x_name = "CTD")
  nrp_upload(data = data, conn = conn, commit = TRUE)

  db_data <- nrp_load_ctd(start_date = NULL, end_date = NULL, sites = NULL, conn = conn)

  expect_is(data, "tbl_df")
  expect_identical(data, db_data)

  db_data <- nrp_load_ctd(start_date = "2018-08-27 16:07:03",
                          end_date = "2018-08-27 16:53:11",
                          sites = NULL, conn = conn)
  expect_is(data, "tbl_df")
  expect_identical(length(data), 14L)
  expect_identical(nrow(db_data), 3536L)
  readwritesqlite::rws_close_connection(conn = conn)

})

