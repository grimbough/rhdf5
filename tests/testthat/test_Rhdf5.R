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

  expect_s3_class(objects_frame <- h5listIdentifier(), "data.frame")
  expect_type(valid_objects <- h5validObjects(), "list")

  expect_shape(objects_frame, dim = c(1L, 2L))
  expect_length(valid_objects, 1L)

  ## create another objects
  sid <- H5Screate()

  expect_shape(h5listIdentifier(), dim = c(2L, 2L))
  expect_length(h5validObjects(), 2L)

  ## now close them
  H5Sclose(sid)
  H5Fclose(fid)

  expect_shape(h5listIdentifier(), dim = c(0L, 2L))
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
