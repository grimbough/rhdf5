library(rhdf5)

############################################################
context("h5set_extent")
############################################################

## output file name
h5File <- tempfile(pattern = "ex_set_extent_", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

## populate an example file
D <- 1:20
h5createFile(h5File)
h5createDataset(file = h5File, dataset = "foo", dims = c(1,length(D)), maxdims = c(4,length(D)))
h5write(obj = D, file = h5File, name = "foo")

test_that("Dimensions as expected", {
    expect_equal( dim(h5read(h5File, name = "foo")), c(1,length(D)) )
})

test_that("Changing dataset dimensions", {
    expect_true( h5set_extent(file = h5File, dataset = "foo", dims = c(2, length(D))))
    expect_equal( dim(h5read(h5File, name = "foo")), c(2,length(D)) )
})

test_that("Fail if given a group", {
    expect_true( h5createGroup(file = h5File, group = "baa") )
    expect_error( h5set_extent(file = h5File, dataset = "baa", dims = c(1,1)), 
                  regexp = "is not a dataset\\.$" )
})

test_that("Fail if missing", {
    expect_error( h5set_extent(file = h5File, dataset = "missing", dims = c(1,1)), 
                  regexp = "does not exist in this HDF5 file.$" )
})

test_that("No open HDF5 objects are left", {
    expect_equal( length(h5validObjects()), 0 )
})

