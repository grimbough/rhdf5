library(rhdf5)

############################################################
context("Testing file locking")
############################################################

test_that("File or directory can be passed", {
  file <- tempfile()
  dir <- tempdir()
  expect_silent(h5testFileLocking(file)) %>%
    expect_is('logical')
  expect_silent(h5testFileLocking(dir)) %>%
    expect_is('logical')
  
  ## Temporary file removed
  expect_false(file.exists(file))
})

test_that("Error when using existing file", {
  
  tf <- tempfile()
  file.create(tf)
  expect_error(h5testFileLocking(location = tf),
               "Testing file locking will remove")
  
})

test_that("Error when missing argument", {
  expect_error(h5testFileLocking(),
               "You must provide a location to test")
})


############################################################
context("Setting file locking")
############################################################

## record the current state of this, we'll set again at the end
curr <- Sys.getenv("HDF5_USE_FILE_LOCKING")

test_that("Disabling sets value", {
  h5disableFileLocking()
  expect_equal(Sys.getenv("HDF5_USE_FILE_LOCKING"), "FALSE")
})

test_that("Enabling removes value", {
  h5enableFileLocking()
  expect_equal(Sys.getenv("HDF5_USE_FILE_LOCKING"), "")
})

## set original value for environment variable
Sys.setenv(HDF5_USE_FILE_LOCKING = curr)
