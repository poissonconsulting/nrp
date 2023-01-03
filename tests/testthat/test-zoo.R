test_that("nrp_read_zooplankton_file works", {

  conn <- nrp_create_db(path = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))

  path <- system.file("extdata", "zooplankton/Arzp20.xlsx",
                      package = "nrp", mustWork = TRUE)

  wrong_path <- system.file("extdata", "ar-empty.rtf", package = "nrp", mustWork = TRUE)

  data <- nrp_read_zooplankton_file(path = path, db_path = conn) %>%
    suppressWarnings()

  expect_is(data, "tbl_df")
  expect_identical(nrow(data), 60L)
  check_zoo_raw_data(data)

  expect_error(nrp_read_zooplankton_file(path = path, db_path = conn, system = "columbia"),
               "'system' must be one of 'arrow', 'kootenay'.")

  expect_error(nrp_read_zooplankton_file(path = wrong_path, db_path = conn),
               "Please ensure input data is a valid excel spreadsheet \\(.xlsx\\).")
})

test_that("nrp_read_zooplankton works", {

  conn <- nrp_create_db(path = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))

  path <- system.file("extdata", "zooplankton",
                      package = "nrp", mustWork = TRUE)
  data <- nrp_read_zooplankton(path, db_path = conn)

  expect_is(data, "tbl_df")
  expect_identical(length(data), 164L)
  expect_identical(nrow(data), 132L)

  expect_error(nrp_read_mysid("not-a-path", db_path = conn),
               "path 'not-a-path' must exist")

  path <- system.file("extdata",
                      package = "nrp", mustWork = TRUE)
  data <- nrp_read_mysid(path, db_path = conn)
  expect_identical(data,  list(x = 1)[-1])
})

test_that("nrp_upload_zooplankton and nrp_download_zoo_sample works", {

  conn <- nrp_create_db(path = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))

  path <- system.file("extdata", "zooplankton/Arzp20.xlsx",
                      package = "nrp", mustWork = TRUE)

  data <- nrp_read_zooplankton_file(path = path, db_path = conn) %>%
    suppressWarnings()

  nrp_upload_zooplankton(data = data, db_path = conn)

  db_data <- readwritesqlite::rws_read_table("Zooplankton", conn = conn)
  expect_identical(length(db_data), 7L)
  expect_identical(nrow(db_data), 8760L)

  db_sample <- readwritesqlite::rws_read_table("ZooplanktonSample", conn = conn)
  expect_identical(length(db_sample), 12L)
  expect_identical(nrow(db_sample), 60L)

  nrp_upload_zooplankton(data = data, db_path = conn, replace = TRUE)

  db_data <- readwritesqlite::rws_read_table("Zooplankton", conn = conn)
  expect_identical(length(db_data), 7L)
  expect_identical(nrow(db_data), 8760L)

  expect_error(nrp_upload_zooplankton(data = data, db_path = conn),
               "UNIQUE constraint failed: ZooplanktonSample.Date, ZooplanktonSample.SiteID, ZooplanktonSample.Replicate, ZooplanktonSample.FileName")
  db_data <- nrp_download_zooplankton(start_date = "2020-04-01",
                                      end_date = "2020-04-25", db_path = conn)

  expect_identical(length(db_data), 150L)
  expect_identical(nrow(db_data), 12L)

  db_data <- nrp_download_zooplankton(start_date = "2020-04-01",
                                      end_date = "2020-04-25",
                                      counts = TRUE,
                                      db_path = conn)

  expect_identical(length(db_data), 150L)
  expect_identical(nrow(db_data), 12L)
  expect_true(all(is.na(db_data$BBosm)))

  expect_error(nrp_download_zooplankton(start_date = "2020-04-01",
                                        end_date = "2020-04-25", db_path = conn,
                                        sites = "wrong"),
               "1 or more invalid site names")

  expect_error(nrp_download_zooplankton(start_date = "2020-04-01",
                                        end_date = "2020-04-25", db_path = conn,
                                        parameters = "wrong"),
               "1 or more invalid parameter names")

})

