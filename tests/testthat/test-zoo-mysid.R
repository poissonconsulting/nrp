
conn <- nrp_create_db(path = ":memory:", ask = FALSE)
teardown(DBI::dbDisconnect(conn))

test_that("nrp_read_zooplankton_file works", {

  path <- system.file("extdata", "zooplankton/Arzp20.xlsx",
                      package = "nrp", mustWork = TRUE)

  wrong_path <- system.file("extdata", "mysid/KLFmys20.xlsx",
                                      package = "nrp", mustWork = TRUE)

  data <- nrp_read_zooplankton_file(path = path, db_path = conn)

  expect_is(data, "tbl_df")
  expect_identical(nrow(data), 60L)
  check_zoo_raw_data(data)


  expect_error(nrp_read_zooplankton_file(path = path, db_path = conn, system = "columbia"),
               "'system' must be one of 'arrow', 'kootenay'.")

  expect_error(nrp_read_zooplankton_file(path = wrong_path, db_path = conn),
               "Columns in data do not match template for zooplankton raw data. see `nrp::zoo_cols` for correct column names and order.")

})


test_that("nrp_read_mysid_file works", {

  path <- system.file("extdata", "mysid/KLFmys20.xlsx",
                      package = "nrp", mustWork = TRUE)

  wrong_path <- system.file("extdata", "zooplankton/Arzp20.xlsx",
                      package = "nrp", mustWork = TRUE)

  data <- nrp_read_mysid_file(path = path, db_path = conn)

  expect_is(data, "tbl_df")
  expect_identical(nrow(data), 60L)
  check_mysid_raw_data(data)


  expect_error(nrp_read_mysid_file(path = path, db_path = conn, system = "columbia"),
               "'system' must be one of 'arrow', 'kootenay'.")

  expect_error(nrp_read_mysid_file(path = wrong_path, db_path = conn),
               "Columns in data do not match template for mysid raw data. see `nrp::mysid_cols` for correct column names and order.")

})
