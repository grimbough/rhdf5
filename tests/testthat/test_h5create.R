library(rhdf5)

context("h5createFile")

## output file name
h5File <- tempfile(pattern = "ex_save", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

test_that("Default arguments", {
    expect_true( h5createFile(file = h5File) )
    expect_true( file.exists(h5File) )
})

test_that("Don't overwrite existing file", {
    expect_message(h5createFile(file = h5File), 
                   regexp = "already exists.\n$")
})

context("h5createGroup")

if(!file.exists(h5File))
    h5createFile(file = h5File)

test_that("Create simple group", {
    expect_true( h5createGroup(file = h5File, group = "foo") )
})

test_that("Create group heirachy", {
    expect_true( h5createGroup(file = h5File, group = "foo/baa") )
})

test_that("Fail if toplevel group missing", {
    ## this is really an error, but doesn't get thrown as one
    expect_error( h5createGroup(file = h5File, group = "baa/foo") )
    H5close()
})


context("h5createDataset")

if(file.exists(h5File))
    file.remove(h5File)
## create empty file
h5createFile(file = h5File)

test_that("Create single dataset", {
    expect_true( h5createDataset(file = h5File, dataset = "A", dims = c(2,1)) )
    expect_true( "A" %in% names(h5dump(file = h5File)) )
    A <- h5read(file = h5File, name = "A")
    
    expect_is(A, "matrix")
    expect_true(nrow(A) == 2)
    expect_true(ncol(A) == 1)
    expect_is(A[1,1], "numeric")
    
})

test_that("Create second integer dataset", {
    expect_true( h5createDataset(file = h5File, dataset = "B", dims = c(4,5), storage.mode = "integer") )
    expect_true( "B" %in% names(h5dump(file = h5File)) )
    B <- h5read(file = h5File, name = "B")
    
    expect_is(B, "matrix")
    expect_true(nrow(B) == 4)
    expect_true(ncol(B) == 5)
    expect_is(B[1,1], "integer")
    
})

test_that("Datasets with different compression levels", {
    
    dataMatrix <- matrix(runif(n = 1e6), nrow = 100000, ncol = 10)
    
    h5File_0 <- tempfile(pattern = "level0_", fileext = ".h5")
    if(file.exists(h5File_0)) 
        file.remove(h5File_0)
    h5createFile(h5File_0)
    expect_true( h5createDataset(file = h5File_0, dataset = "A", dims = dim(dataMatrix), level = 0) )
    h5write( dataMatrix, file = h5File_0, name = "A")
    
    h5File_9 <- tempfile(pattern = "level9_", fileext = ".h5")
    if(file.exists(h5File_9)) 
        file.remove(h5File_9)
    h5createFile(h5File_9)
    expect_true( h5createDataset(file = h5File_9, dataset = "A", dims = dim(dataMatrix), level = 9) )
    h5write( dataMatrix, file = h5File_9, name = "A")
    ## expect compressed file to be at least a small as uncompressed
    expect_lte( file.size(h5File_9), file.size(h5File_0) )
})