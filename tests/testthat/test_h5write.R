library(rhdf5)

context("h5save function")
## h5save doesn't close files, so we have to use H5close() after every call!

A = 1:7;  
## output file name
h5File <- tempfile(pattern = "ex_save", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

test_that("Error if file doesn't exist", {
    expect_error( h5write(obj = A, file = h5File, name = "A") )
})

test_that("Writing works", {
    expect_true( h5createFile(h5File) )
    expect_true( file.exists(h5File) )
    h5write(obj = A, file = h5File, name = "A")
    expect_equal( H5read(file = h5File, name = "A"), A )
})


test_that("No open HDF5 objects are left", {
    expect_equal( length(h5validObjects()), 0 )
})