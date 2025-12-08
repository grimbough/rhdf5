library(rhdf5)

############################################################
context("Utility functions")
############################################################

test_that("Printing library versions", {
  expect_message(
    h5version(),
    regexp = "This is Bioconductor rhdf5 [0-9.]+ linking to C-library HDF5 [0-9.]+"
  )
})

test_that("We can list created objects", {
  fid <- H5Fcreate(name = withr::local_tempfile())

  expect_is(objects_frame <- h5listIdentifier(), "data.frame")
  expect_is(valid_objects <- h5validObjects(), "list")

  expect_identical(dim(objects_frame), c(1L, 2L))
  expect_length(valid_objects, 1L)

  ## create another objects
  sid <- H5Screate()

  expect_identical(dim(h5listIdentifier()), c(2L, 2L))
  expect_length(h5validObjects(), 2L)

  ## now close them
  H5Sclose(sid)
  H5Fclose(fid)

  expect_identical(dim(h5listIdentifier()), c(0L, 2L))
  expect_length(h5validObjects(), 0)
})

# test_that("Find location of libray", {
#
#    expect_output( Rhdf5lib:::pkgconfig(), regexp = "libhdf5" )
#
#    path <- capture.output( Rhdf5lib:::pkgconfig() )
#    if( Sys.info()[['sysname']] != "Windows" ) {
#        libfile <- gsub(pattern = "^-l", replacement = "", x = path)
#        expect_true( file.exists(libfile) )
#    }
# })
