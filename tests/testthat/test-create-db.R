context("create-db")

test_that("nrp-create-db works", {

  conn <- nrp_create_db(path  = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))

  expect_identical(readwritesqlite::check_sqlite_connection(conn, connected = TRUE), conn)
})
