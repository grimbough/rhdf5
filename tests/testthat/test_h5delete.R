library(rhdf5)

############################################################
context("h5delete")
############################################################

A = 1:7;  
## output file name
h5File <- tempfile(pattern = "ex_delete", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)
h5write(obj = A, file = h5File, name = "A")

test_that("Error if file doesn't exist", {
    expect_error( h5delete(file = "foo", name = "baa"), regexp = "Cannot open file" )
})

test_that("Error if link doesn't exist", {
    expect_error( h5delete(file = h5File, name = "baa"), regexp = "Specified link doesn't exist" )
})

test_that("Deletion works for vector", {

    expect_true( "A" %in% h5ls(h5File)$name )
    expect_silent( h5delete(file = h5File, name = "A") )    
    expect_false( "A" %in% h5ls(h5File)$name )
})

## add several datatypes including a list with subgroups 
fid <- H5Fopen(h5File)
expect_silent(h5writeDataset(obj = matrix(1:10, ncol = 2), h5loc = fid, name = "matrix"))
expect_silent(h5writeDataset(obj = list(a = 1:4, b = letters[10:15]), h5loc = fid, name = "list"))
expect_silent(h5writeDataset(obj = data.frame("col_A" = 1:10, "col_B" = letters[1:10]), 
                             h5loc = fid, name = "data.frame"))
H5Fclose(fid)

## record the file size
original_filesize <- file.size(h5File)

test_that("Deletion is selective", {
    
    expect_true( "data.frame" %in% h5ls(h5File)$name )
    expect_silent( h5delete(file = h5File, name = "data.frame") )  
    expect_false( "data.frame" %in% h5ls(h5File)$name )
    expect_true( "list" %in% h5ls(h5File)$name )
})

test_that("Deletion removes subgroups", {
    
    expect_silent( h5delete(file = h5File, name = "list") )  
    expect_false( "a" %in% h5ls(h5File)$name )
})

test_that("Deletion reduces filesize", {
    
    expect_lt( file.size(h5File), original_filesize )
      
})
    
test_that("No open HDF5 objects are left", {
    expect_equal( length(h5validObjects()), 0 )
})
