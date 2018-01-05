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
    expect_error( h5write(obj = A, file = h5File, name = "A", createnewfile = FALSE) )
})

test_that("Writing works", {
    #expect_true( h5createFile(h5File) )
    #expect_true( file.exists(h5File) )
    ## writing to a file name
    expect_silent( h5write(obj = A, file = h5File, name = "A") )
    expect_equal( as.integer(h5read(file = h5File, name = "A")), A )
    
    fid <- H5Fopen(name = h5File)
    ## writing to a group
    gid <- H5Gcreate(h5loc = fid, name = "test_group")
    expect_silent( h5write(matrix(1:20, ncol = 10), file = gid, name = "foo") )
    H5Gclose(gid)
    
    H5Fclose(fid)
})


test_that("Attributes are written too", {
    B <- runif(n = 10)
    attr(B, "scale") <- "centimeters" 
    h5write(obj = B, file = h5File, name = "B", write.attributes = TRUE)
    ## note that attributes aren't retrieved here
    expect_equivalent( as.numeric(h5read(file = h5File, name = "B")), B )
    #expect_equal( h5read(file = h5File, name = "B", read.attributes = TRUE), B )
    expect_true( "scale" %in% names(h5readAttributes(file = h5File, name = "B")) )
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
    expect_silent(h5writeDataset(obj = logical(1), h5loc = fid, name = "logical"))
    expect_silent(h5writeDataset(obj = list(a = 1:4, b = letters[10:15]), h5loc = fid, name = "list"))
    expect_silent(h5writeDataset(obj = data.frame("col_A" = 1:10, "col_B" = letters[1:10]), 
                                 h5loc = fid, name = "data.frame"))
    expect_silent(h5writeDataset(obj = data.frame("col_A" = 1:10, "col_B" = letters[1:10]), 
                                 h5loc = fid, name = "data.frame2", DataFrameAsCompound = FALSE))
    H5Fclose(fid)
})

############################################################
context("Writing a datset subset")
############################################################

## output file name
h5File <- tempfile(pattern = "ex_writeDataset_", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

test_that("Overwriting a subset", {
    fid <- H5Fcreate(name = h5File)
    expect_silent(h5writeDataset(obj = matrix(1:40, ncol = 4), h5loc = fid, name = "matrix"))
    expect_silent(h5writeDataset(obj = rep(0, 20), h5loc = fid, name = "matrix", index = list(NULL, 2:3)))
    H5Fclose(fid)
    
    expect_is( mat <- h5read(h5File, name = "matrix"), "matrix" )
    expect_true( all(mat[,2] == 0) )
    
})


## remove this later
#H5close()

test_that("No open HDF5 objects are left", {
    expect_equal( length(h5validObjects()), 0 )
})
