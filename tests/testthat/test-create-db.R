test_that("nrp-create-db works", {
  conn <- nrp_create_db(path = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))

  readwritesqlite::rws_list_tables(conn)

  tables <- c(
    "BasinArm", "CTD", "Lake", "metalsEMS", "Mysid", "MysidSample",
    "Phytoplankton", "PhytoplanktonSample", "PhytoplanktonSpecies",
    "Sites", "standardEMS", "VisitCTD", "Zooplankton", "ZooplanktonSample"
  )

  expect_identical(sort(readwritesqlite::rws_list_tables(conn)), sort(tables))

  expect_null(readwritesqlite::chk_sqlite_conn(conn, connected = TRUE))
})
