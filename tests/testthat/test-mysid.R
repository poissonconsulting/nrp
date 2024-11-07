test_that("nrp_read_mysid_file works", {
  conn <- nrp_create_db(path = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))

  path <- system.file("extdata", "mysid/KLFmys20.xlsx",
    package = "nrp", mustWork = TRUE
  )

  wrong_path <- system.file("extdata", "ar-empty.rtf",
    package = "nrp", mustWork = TRUE
  )

  data <- nrp_read_mysid_file(path = path, db_path = conn) %>%
    suppressWarnings()

  expect_is(data, "tbl_df")
  expect_identical(nrow(data), 60L)
  check_mysid_raw_data(data)

  expect_error(
    nrp_read_mysid_file(path = path, db_path = conn, system = "columbia"),
    "'system' must be one of 'arrow', 'kootenay'."
  )

  expect_error(
    nrp_read_mysid_file(path = wrong_path, db_path = conn),
    "Please ensure input data is a valid excel spreadsheet \\(.xlsx\\)."
  )
})

test_that("nrp_read_mysid works", {
  conn <- nrp_create_db(path = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))

  path <- system.file("extdata", "mysid",
    package = "nrp", mustWork = TRUE
  )
  data <- nrp_read_mysid(path, db_path = conn)

  expect_is(data, "tbl_df")
  expect_identical(length(data), 50L)
  expect_identical(nrow(data), 78L)

  expect_error(
    nrp_read_mysid("not-a-path", db_path = conn),
    "path 'not-a-path' must exist"
  )

  path <- system.file("extdata",
    package = "nrp", mustWork = TRUE
  )
  data <- nrp_read_mysid(path, db_path = conn)
  expect_identical(data, list(x = 1)[-1])
})

test_that("nrp_upload_mysid and nrp_download_mysid_visit works", {
  conn <- nrp_create_db(path = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))

  path <- system.file("extdata", "mysid/KLFmys20.xlsx",
    package = "nrp", mustWork = TRUE
  )

  data <- nrp_read_mysid_file(path = path, db_path = conn) %>%
    suppressWarnings()

  nrp_upload_mysid(data = data, db_path = conn)

  db_data <- readwritesqlite::rws_read_table("Mysid", conn = conn)
  expect_identical(length(db_data), 5L)
  expect_identical(nrow(db_data), 2040L)

  db_sample <- readwritesqlite::rws_read_table("MysidSample", conn = conn)
  expect_identical(length(db_sample), 15L)
  expect_identical(nrow(db_sample), 60L)

  nrp_upload_mysid(data = data, db_path = conn, replace = TRUE)

  db_data <- readwritesqlite::rws_read_table("Mysid", conn = conn)
  expect_identical(length(db_data), 5L)
  expect_identical(nrow(db_data), 2040L)

  expect_error(
    nrp_upload_mysid(data = data, db_path = conn),
    "UNIQUE constraint failed: MysidSample.Date, MysidSample.SiteID, MysidSample.Replicate"
  )

  db_data <- nrp_download_mysid(
    start_date = "2020-04-01",
    end_date = "2020-04-25", db_path = conn
  )

  expect_identical(length(db_data), 37L)
  expect_identical(nrow(db_data), 24L)

  expect_error(
    nrp_download_mysid(
      start_date = "2020-04-01",
      end_date = "2020-04-25", db_path = conn,
      sites = "wrong"
    ),
    "1 or more invalid site names"
  )

  expect_error(
    nrp_download_mysid(
      start_date = "2020-04-01",
      end_date = "2020-04-25", db_path = conn,
      parameters = "wrong"
    ),
    "1 or more invalid parameter names"
  )
})
