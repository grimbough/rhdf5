library(rhdf5)

############################################################
context("h5dump")
############################################################

A = 1:7;  
B = 1:18; 
D = seq(0,1,by=0.1)
## output file name
h5File <- tempfile(pattern = "ex_dump", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

# create file with group heirachy  
h5createFile(h5File)
h5createGroup(file = h5File, group = "foo")
h5createDataset(file = h5File, dataset = "foo/A", dims = c(1, length(A) ))
h5write(obj = A, file = h5File, name = "foo/A")
h5createDataset(file = h5File, dataset = "foo/B", dims = c(1,length(B) ))
h5write(obj = B, file = h5File, name = "foo/B")
h5createDataset(file = h5File, dataset = "baa", dims = c(1,length(D) ))
h5write(obj = D, file = h5File, name = "baa")

test_that("Default arguments", {
    
    dump_output <- h5dump( file = h5File )
    expect_is( dump_output, "list" )
    expect_true( all(c("baa", "foo") %in% names(dump_output)) )
    
    expect_is( dump_output$baa, "matrix" )
    
})

test_that("Check reading only dataset headers", {
    
    dump_output <- h5dump( file = h5File, load = FALSE )
    expect_is( dump_output$baa, "data.frame" )
    
})

test_that("No recursion", {
    
    expect_null( h5dump( file = h5File, recursive = FALSE )$foo )
    
})

test_that("Changing traversal order", {
    expect_identical(names(h5dump(h5File, order = "H5_ITER_DEC")), c("foo", "baa"))
    expect_identical(names(h5dump(h5File, order = "H5_ITER_INC")), c("baa", "foo"))
})

test_that("No open HDF5 objects are left", {
    expect_equal( length(h5validObjects()), 0 )
})
