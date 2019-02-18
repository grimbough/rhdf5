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

test_that("Write by index and hyperslab works.", {
    h5createDataset(file = h5File, dataset = "D", dims = c(10, 9))

    ## Test index works
    h5write(1:4, file = h5File, name = "D", index = list(2:3, c(3, 6)))
    res <- h5read(file = h5File, name = "D", index = list(2:3, c(3, 6)))
    expect_equal(res, matrix(1:4, nrow = 2, ncol = 2))

    ## Test hyperslabs without stride and block arguments work
    h5write(matrix(1:12, nrow = 4, ncol = 3), file = h5File, name = "D", 
        start = c(2, 2), stride = NULL, count = c(4, 3), block = NULL)
    expect_equal(h5read(h5File, "D")[2:5, 2:4], matrix(1:12, nrow = 4, ncol = 3))
    res <- h5read(file = h5File, name = "D", start = c(2, 2), stride = NULL,
        count = c(4, 3), block = NULL)
    expect_equal(res, matrix(1:12, nrow = 4, ncol = 3))

    ## Test hyperslabs with all arguments work
    h5write(matrix(1:48, nrow = 6, ncol = 8), file = h5File, name = "D", 
        start = c(1, 1), stride = c(4, 5), count = c(2, 2), block = c(3, 4))
    expect_equal(h5read(h5File, "D")[c(1:3, 5:7), c(1:4, 6:9)],
        matrix(1:48, nrow = 6, ncol = 8))
    res <- h5read(file = h5File, name = "D", start = c(1, 1), stride = c(4, 5),
        count = c(2, 2), block = c(3, 4))
    expect_equal(res, matrix(1:48, nrow = 6, ncol = 8))
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
context("h5write with data.frames")
############################################################

h5f1 <- tempfile(fileext = ".h5")
h5f2 <- tempfile(fileext = ".h5")

h5createFile(h5f1)
h5createFile(h5f2)

## we use file size as a proxy for chunk size
## larger chunks should compress better in this example
test_that("Changing chunk size works", {
    expect_silent(h5write(data.frame("A" = rep(1:100), "B" = 201:300),
                          h5f1, "dset", level = 7, chunk = 1L))
    expect_silent(h5write(data.frame("A" = rep(1:100), "B" = 201:300),
                          h5f2, "dset", level = 7, chunk = 100L))
    expect_true(file.size(h5f2) < file.size(h5f1))
})

if(.Platform$r_arch != "i386") {
    counts_1 <- tryCatch(rep(0, 500000000),
		error = function(e) NULL)

    if(!is.null(counts_1)) { 
		d1 <- data.frame(counts_1, counts_1, counts_1, counts_1)
		test_that("Very large data.frames are limited to chunk size < 4GB", {
		    fid <- H5Fcreate(name = h5File)
		    expect_silent(did <- .Call("_h5createDataFrame", d1, fid@ID, "test", 7L, nrow(d1), PACKAGE='rhdf5'))
		    expect_gt(as.numeric(did), 0)
		    expect_equal(.Call("_H5Dclose", did, PACKAGE='rhdf5'), 0)
		    H5Fclose(fid)
		})
    }
}


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
