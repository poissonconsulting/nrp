context("ctd")

test_that("nrp_read_ctd_file works", {

  conn <- nrp_create_db(path = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))

  path <-  system.file("extdata", "ctd/2018/KL1_27Aug2018008downcast.cnv", package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd_file(path, db_path = conn)

  check_ctd_data(data, exclusive = TRUE, order = TRUE)
  expect_is(data, "tbl_df")
  expect_identical(nrow(data), 1413L)

  path <-  system.file("extdata", "bad ctd/2018/KL_Badly_named_file_1.cnv",
                       package = "nrp", mustWork = TRUE)
  expect_error(data <- nrp_read_ctd_file(path, db_path = conn),
               "Station name could not be extracted from file name: No matches")

})

test_that("nrp_read_ctd_file w/ read.table alternative works", {

  conn <- nrp_create_db(path = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))
  path <-  system.file("extdata", "bad ctd/2018/AR6_Apr_26_2011.cnv", package = "nrp", mustWork = TRUE)

  data <- nrp_read_ctd_file(path, db_path = conn)

  expect_is(data, "tbl_df")
  check_ctd_data(data, exclusive = TRUE, order = TRUE)
  expect_identical(length(data), 18L)
  expect_identical(nrow(data), 1943L)

})


test_that("nrp_read_ctd works", {

  conn <- nrp_create_db(path = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))

  path <-  system.file("extdata", "ctd/2018", package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd(path, db_path = conn)

  expect_is(data, "tbl_df")
  check_ctd_data(data, exclusive = TRUE, order = TRUE)
  expect_identical(length(data), 18L)
  expect_identical(nrow(data), 1413L)


  path <-  system.file("extdata", "ctd/2018", package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd(path, db_path = conn,  recursive = TRUE)

  expect_is(data, "tbl_df")
  check_ctd_data(data, exclusive = TRUE, order = TRUE)
  expect_identical(length(data), 18L)
  expect_identical(nrow(data), 5329L)

  path <-  system.file("extdata", "ctd", package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd(path, db_path = conn)
  expect_identical(data,  list(x = 1)[-1])

})

test_that("nrp_download_sites works", {

  conn <- nrp_create_db(path  = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))

  sites_db <- nrp_download_sites(db_path = conn)
  sites_raw_data <- nrp::ctdSites

  expect_identical(nrow(sites_db), nrow(sites_raw_data))
  expect_identical(names(sites_db), names(sites_raw_data))
})

test_that("nrp_download_ctd_visit works", {

  conn <- nrp_create_db(path  = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))

  sites_db <- nrp_download_ctd_visit(db_path = conn)
  expect_is(sites_db, "tbl_df")

})

test_that("nrp_download_ctd_basin_arm works", {

  conn <- nrp_create_db(path  = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))

  basin_arm_db <- nrp_download_ctd_basin_arm(db_path = conn)
  basin_arm_raw_data <- nrp::basinArm

  expect_identical(nrow(basin_arm_db), nrow(basin_arm_raw_data))
  expect_identical(names(basin_arm_db), names(basin_arm_raw_data))
})

test_that("nrp_add_ctd_sites works", {

  new_data <- tibble(SiteID = "NewID", EmsSiteNumber = "NewNumber", SiteName = "New Site Name",
                     EmsSiteName = "name ems",BasinArm = "Upper", MaxDepth = 100, Easting = 434792,
                     Northing = 5605351)

  conn <- nrp_create_db(path = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))

  old_sites <- nrp_download_sites(db_path = conn)

  nrp_add_ctd_sites(data = new_data, db_path = conn)

  updated_sites <- nrp_download_sites(db_path = conn)
  sf::st_geometry(updated_sites) <- NULL

  expect_equal(nrow(old_sites) + 1, nrow(updated_sites))

})

test_that("nrp_upload_ctd works", {

  conn <- nrp_create_db(path  = ":memory:", ask = FALSE)
  path <-  system.file("extdata", "ctd/2018/KL1_27Aug2018008downcast.cnv",
                       package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd_file(path = path, db_path = conn)

  nrp_upload_ctd(data = data, db_path = conn)
  db_data <- readwritesqlite::rws_read_table("CTD", conn = conn)
  readwritesqlite::rws_disconnect(conn = conn)

  expect_identical(length(db_data), 16L)
  expect_identical(nrow(db_data), 1282L)

  conn <- nrp_create_db(path  = ":memory:", ask = FALSE)

  expect_error(nrp_upload_ctd(data = data, db_path = "wrong_path.sqlite"),
               "path 'wrong_path.sqlite' must exist")
  readwritesqlite::rws_disconnect(conn = conn)

  conn <- nrp_create_db(path  = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))

  wrong_file <- path
  expect_error(nrp_upload_ctd(data = data, db_path = wrong_file),
               "File provided is not an SQLite database")
})


test_that("nrp_upload_ctd(replace = TRUE) works", {

  conn <- nrp_create_db(path  = ":memory:", ask = FALSE)
  path <-  system.file("extdata", "ctd/2018/KL1_27Aug2018008downcast.cnv",
                       package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd_file(path = path, db_path = conn)
  data %<>% mutate(Flag = 1)
  nrp_upload_ctd(data = data, db_path = conn)

  data <- nrp_read_ctd_file(path = path, db_path = conn)
  nrp_upload_ctd(data = data, db_path = conn, replace = TRUE)

  db_data <- readwritesqlite::rws_read_table("CTD", conn = conn)
  expect_true(all(db_data$Flag == 0))

})

test_that("nrp_download_ctd works", {

  conn <- nrp_create_db(path = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))

  path <-  system.file("extdata", "ctd/2018", package = "nrp", mustWork = TRUE)
  data <- nrp_read_ctd(path, db_path = conn, recursive = TRUE)

  nrp_upload_ctd(data = data, db_path = conn)

  db_data <- nrp_download_ctd(db_path = conn)

  expect_is(data, "tbl_df")

  db_data <- nrp_download_ctd(start_date = "2018-08-26",
                          end_date = "2018-08-28",
                          sites = NULL, db_path = conn)

  expect_is(data, "tbl_df")
  expect_identical(length(db_data), 16L)
  expect_identical(nrow(db_data), 4749L)

})

test_that("getOption nrp.dp_path works", {

  conn <- nrp_create_db(path = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))

  options(nrp.db_path = conn)

  path <-  system.file("extdata", "ctd/2018", package = "nrp", mustWork = TRUE)

  option_data <- nrp_read_ctd(path = path, recursive = TRUE)
  no_option_data <- nrp_read_ctd(path = path, db_path = conn, recursive = TRUE)

  expect_identical(option_data, no_option_data)

})

