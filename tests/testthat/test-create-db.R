context("create-db")

test_that("nrp-create-db works", {

  nrp_create_db("test-nrp.sqlite", ask = FALSE)
  check_file_exists("test-nrp.sqlite")
  unlink("test-nrp.sqlite")

})
