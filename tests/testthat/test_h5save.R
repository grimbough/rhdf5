library(rhdf5)

context("h5save function")
## h5save doesn't close files, so we have to use H5close() after every call!

A = 1:7;  
B = 1:18; 
D = seq(0,1,by=0.1)
## output file name
h5File <- tempfile(pattern = "ex_save", fileext = ".h5")

test_that("Default arguments", {
    h5save(A, B, D, file = h5File)
    H5close()
})

test_that("File exists", {
    expect_true( file.exists(h5File) )
    ## remove it for now, we should check overwriting later
    file.remove(h5File)
})

test_that("Changing dataset names", {
    dsetNames <- c("dset1", "dset2", "dset3")
    h5save(A, B, D, file = h5File, name = dsetNames)
    H5close()
    expect_true( file.exists(h5File) )
    expect_equal( names(h5dump(h5File)),
                 dsetNames )
})

test_that("Fail if file doesn't exist", {
    if(file.exists(h5File))
       file.remove(h5File)
    expect_error(h5save(A, B, D, file = h5File, createnewfile = FALSE))
    H5close()
})

test_that("Adding to existing file", {
    h5save(A, file = h5File)
    H5close()
    expect_equal( names(h5dump(h5File)), "A" )
    
    h5save(B, D, file = h5File)
    H5close()
    expect_equal( names(h5dump(h5File)), c("A", "B", "D") )
})

test_that("Try to overwrite existing data set with same name", {
    if(file.exists(h5File))
        file.remove(h5File)
    
    h5save(A, file = h5File)
    H5close()
    
    ## this will throw an HDF5 error, but I don't know how to catch it yet
    h5save(B, file = h5File, name = "A")
    H5close()
    
    expect_false( identical(h5read(file = h5File, name = "A"), B) )
})

## currently fails hard!
# test_that("Name can take a path", {
#     if(file.exists(h5File))
#         file.remove(h5File)
#     
#     h5save(A, file = h5File, name = "/foo/baa")
#     H5close()
# })

