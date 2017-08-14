library(rhdf5)

############################################################
context("h5write")
############################################################

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
    expect_equal( as.integer(h5read(file = h5File, name = "A")), A )
})


test_that("No open HDF5 objects are left", {
    expect_equal( length(h5validObjects()), 0 )
})

############################################################
context("h5writeDataset")
############################################################

## output file name
h5File <- tempfile(pattern = "ex_writeDataset_", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

test_that("Writing to file handle", {
    fid <- H5Fcreate(name = h5File)
    expect_silent(h5writeDataset(obj = matrix(1:10, ncol = 2), h5loc = fid, name = "matrix"))
    expect_silent(h5writeDataset(obj = integer(1), h5loc = fid, name = "integer"))
    expect_silent(h5writeDataset(obj = numeric(1), h5loc = fid, name = "numeric"))
    expect_silent(h5writeDataset(obj = "foobaa", h5loc = fid, name = "character"))
    expect_silent(h5writeDataset(obj = list(a = 1:4, b = letters[10:15]), h5loc = fid, name = "list"))
    expect_silent(h5writeDataset(obj = data.frame("col_A" = 1:10, "col_B" = letters[1:10]), h5loc = fid, name = "data.frame"))
    H5Fclose(fid)
})

test_that("No open HDF5 objects are left", {
    expect_equal( length(h5validObjects()), 0 )
})
