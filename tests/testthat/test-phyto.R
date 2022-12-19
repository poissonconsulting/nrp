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
