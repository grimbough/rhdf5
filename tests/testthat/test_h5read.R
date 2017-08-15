library(rhdf5)

############################################################
context("h5read")
############################################################

A = 1L:7L;  
B = matrix(1:18, ncol = 2); 
D = seq(0, 1, by=0.1)
attr(D, "scale") <- "centimeters"

## output file name
h5File <- tempfile(pattern = "ex_read", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

# create file with group heirachy  
h5createFile(h5File)
h5createGroup(file = h5File, group = "foo")
h5createDataset(file = h5File, dataset = "foo/A", dims = c(1, length(A) ), storage.mode = "integer")
h5write(obj = A, file = h5File, name = "foo/A")
h5createDataset(file = h5File, dataset = "foo/B", dims = c(2, length(B)/2 ))
h5write(obj = B, file = h5File, name = "foo/B")
h5createDataset(file = h5File, dataset = "baa", dims = c(1, length(D) ))
h5write(obj = D, file = h5File, name = "baa", write.attributes = TRUE)

test_that("Reading a dataset", {
    
    baa <- h5read(h5File, name = "baa")
    expect_is( baa, "matrix" )
    expect_equal( dim(baa), c(1, length(D)) )
    
})

test_that("Reading a group", {
    
    foo <- h5read(h5File, name = "foo")
    expect_is( foo, "list" )
    expect_equal( length(foo), 2 )
    expect_true( all(c("A", "B") %in% names(foo)) )
})

test_that("Reading a nested dataset", {
    
    fooA <- h5read(h5File, name = "foo/A")
    expect_is( fooA, "matrix" )
    expect_equal( dim(fooA), c(1, length(A)) )
})

test_that("Dropping dimensions", {
    
    fooA <- h5read(h5File, name = "foo/A", drop = TRUE)
    expect_is( fooA, "integer" )
    expect_null( dim(fooA) )
    expect_equal( fooA, A )
    
    ## this drops for matrices too
    fooB <- h5read(h5File, name = "foo/B", drop = TRUE)
    expect_is( fooB, "numeric" )
    expect_null( dim(fooB) )
    expect_equal( fooB, as.numeric(B) )
})

test_that("Reading attributes too", {
    
    baa <- h5read(h5File, name = "baa", read.attributes = TRUE)
    expect_equal( as.character(attributes(baa)$scale), attributes(D)$scale )
})

test_that("Error if file doesn't exist", {
    
    expect_error( h5read(file = "/foo/baa.h5", name = "missing"),
                  regexp = "does not exist.$")
    
})

test_that("Error if asking for something that isn't there", {
    
    expect_error( h5read(file = h5File, name = "missing"),
                  regexp = "does not exist in this HDF5 file.$")
    
})

test_that("No open HDF5 objects are left", {
    expect_equal( length(h5validObjects()), 0 )
})

