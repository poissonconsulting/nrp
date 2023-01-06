test_that("nrp_add_phyto_species works", {

  spp <- tibble(
    Taxa = "New Taxa",
    Genus = "New Genus",
    ClassName = "New Class",
    ClassAlias = NA_character_
  )

  conn <- nrp_create_db(path = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))

  old_spp <- nrp_download_phyto_species(db_path = conn)

  nrp_add_phyto_species(data = spp, db_path = conn)

  updated_spp <- nrp_download_phyto_species(db_path = conn)
  spp_db <- setdiff(updated_spp, old_spp)

  expect_identical(spp, spp_db)

})

test_that("nrp_read_plytoplankton_file works", {

  conn <- nrp_create_db(path = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))

  path <- system.file("extdata", "phyto/phyto1.xlsx",
                      package = "nrp", mustWork = TRUE)

  wrong_path <- system.file("extdata", "ar-empty.rtf", package = "nrp", mustWork = TRUE)

  data <- nrp_read_phyto_file(path = path, db_path = conn) %>%
    suppressWarnings()

  expect_warning(
    nrp_read_phyto_file(path = path, db_path = conn),
    "The input data is missing the following columns which will be assigned NA:  Count_Number, Species_Bvol, Biomass"
    )

  expect_is(data, "tbl_df")
  expect_identical(nrow(data), 5L)
  expect_identical(length(data), 13L)

  expect_error(nrp_read_phyto_file(path = wrong_path, db_path = conn),
               "Please ensure input data is a valid excel spreadsheet \\(.xlsx\\).")

  path2 <- system.file("extdata", "phyto/bad/phyto2.xlsx",
                      package = "nrp", mustWork = TRUE)

  expect_warning(nrp_read_phyto_file(path = path2, db_path = conn),
               "Sites in input data not present in 'Sites' table in database: 'AR12'.")

  path3 <- system.file("extdata", "phyto/bad/phyto3.xlsx",
                       package = "nrp", mustWork = TRUE)

  expect_warning(nrp_read_phyto_file(path = path3, db_path = conn),
                 "Taxa in input data not present in 'PhytoplanktonSpecies' table in database: 'New Species'.")

})

test_that("nrp_read_phyto works", {

  conn <- nrp_create_db(path = ":memory:", ask = FALSE)
  teardown(DBI::dbDisconnect(conn))

  path <- system.file(
    "extdata", "phyto", package = "nrp", mustWork = TRUE
    )

  data <- nrp_read_phyto(path, db_path = conn)

  expect_is(data, "tbl_df")
  expect_identical(length(data), 13L)
  expect_identical(nrow(data), 5L)

  path <- system.file(
    "extdata", "phyto", "multiple", package = "nrp", mustWork = TRUE
    )

  data <- nrp_read_phyto(path, db_path = conn, recursive = TRUE)

  expect_is(data, "tbl_df")
  expect_identical(length(data), 13L)
  expect_identical(nrow(data), 15L)

  expect_error(
    nrp_read_phyto("not-a-path", db_path = conn), "path 'not-a-path' must exist"
    )

  path <- system.file("extdata", package = "nrp", mustWork = TRUE)
  data <- nrp_read_phyto(path, db_path = conn)
  expect_identical(data,  list(x = 1)[-1])
})
