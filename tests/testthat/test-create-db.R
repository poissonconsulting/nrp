context("create-db")

test_that("nrp-create-db works", {

  conn <- nrp_create_db(path  = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))

  expect_true(readwritesqlite::chk_sqlite_conn(conn, connected = TRUE))
})
