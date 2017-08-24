library(rhdf5)

############################################################
context("H5Fcreate")
############################################################

test_that("Invalid file names", {
    expect_error( H5Fcreate(name = 1), regexp = "must be a character string of length 1")
    expect_error( H5Fcreate(name = c("F1", "F2")), regexp = "must be a character string of length 1")
})

h5File <- tempfile(pattern = "H5F_", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

test_that("Default arguments", {
    expect_silent( fid <- H5Fcreate(name = h5File) )
    expect_is(fid , "H5IdComponent" )
    H5Fclose(fid)
})

## this should really be an error
#test_that("Fail if file already open", {
#    fid <- H5Fopen(name = h5File)
#    expect_message( fid2 <- H5Fcreate(name = h5File), 
#                  regexp = "unable to create file")
#    H5Fclose(fid)
#})

############################################################
context("H5Fis_hdf5")
############################################################

h5File <- tempfile(pattern = "H5F_", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

test_that("Invalid file names", {
    expect_error( H5Fis_hdf5(name = 1), regexp = "must be a character string of length 1")
    expect_error( H5Fis_hdf5(name = c("F1", "F2")), regexp = "must be a character string of length 1")
})

test_that("Missing file", {
    expect_warning(res <- H5Fis_hdf5(name = "/foo/baa.h5"), regexp = "File does not exist.")
    expect_equal(res, NA)
})

test_that("Check if HDF5", {
    
    ## create a new HDF5 file
    if(file.exists(h5File))
        file.remove(h5File)
    expect_silent( fid <- H5Fcreate(name = h5File) )
    H5Fclose(fid)
    ## write a non HDF5 file
    txtFile <- tempfile(fileext = "txt")
    writeLines(text = "foo\nbaa", con = txtFile)
    
    expect_true(H5Fis_hdf5(name = h5File))
    expect_false(H5Fis_hdf5(name = txtFile))
})

test_that("No open HDF5 objects are left", {
    expect_equal( length(h5validObjects()), 0 )
})
