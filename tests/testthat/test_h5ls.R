library(rhdf5)

############################################################
context("h5ls")
############################################################

A = 1:7;  
B = 1:18; 
D = seq(0,1,by=0.1)
## output file name
h5File <- tempfile(pattern = "ex_h5ls_", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

# create file with group heirachy  
h5createFile(h5File)
h5createGroup(file = h5File, group = "foo")
h5createDataset(file = h5File, dataset = "foo/A", dims = c(1, length(A) ), maxdims = c(2, 50))
h5write(obj = A, file = h5File, name = "foo/A")
h5createDataset(file = h5File, dataset = "foo/B", dims = c(1,length(B) ))
h5write(obj = B, file = h5File, name = "foo/B")
h5createDataset(file = h5File, dataset = "baa", dims = c(1,length(D) ))
h5write(obj = D, file = h5File, name = "baa")

test_that("Default arguments", {
    
    ls_output <- h5ls( file = h5File )
    expect_is( ls_output, "data.frame" )
    expect_identical( ls_output$name, c("baa", "foo", "A", "B") )
    expect_equal( dim(ls_output), c(4, 5) )
    
})

test_that("Expanded information", {
    
    ls_output <- h5ls( file = h5File, all = TRUE )
    expect_equal( dim(ls_output), c(4, 12) )
    
})

test_that("h5ls reads dimensions correctly", {
    
    ls_output <- h5ls( file = h5File, all = TRUE )
    expect_equal(ls_output[3, "dim"], "1 x 7")
    expect_equal(ls_output[3, "maxdim"], "2 x 50")
    
})

test_that("Changing recursion depth", {
    
    expect_silent (ls_output <- h5ls(file = h5File, recursive = FALSE) )
    expect_equal( dim(ls_output), c(2, 5) )
    expect_identical( ls_output$name, c("baa", "foo") )
    
    expect_identical( h5ls(h5File, recursive = 1),
                      ls_output )
    expect_identical( h5ls(h5File, recursive = -1),
                      h5ls(h5File) )
    
    expect_error( h5ls(h5File, recursive = 0) )
    expect_warning( h5ls(h5File, recursive = 1:3), 
                    regexp = "'recursive' must be of length 1")
    expect_error( h5ls(h5File, recursive = "TRUE") )
})

test_that("Changing traversal order", {
    expect_identical( h5ls(h5File, order = "H5_ITER_DEC")$name, 
                      c("foo", "B", "A", "baa") )
    expect_identical( h5ls(h5File, order = "H5_ITER_INC")$name,
                      c("baa", "foo", "A", "B") )
})

test_that('Passing H5Identifier does not close it', {
    fid <- H5Fopen(h5File)
    expect_is( h5ls(file = fid), class = 'data.frame')
    expect_silent( H5Fclose(fid) )
})



test_that("h5ls warns if identical groups are detected", {
    
    createFileWithCopiedGroup <- function(h5file) {
        h5createFile(h5file)
        h5createGroup(h5file, group = "/group1")
        h5createDataset(h5file, dataset = "/dset1", dims = list(50))
        h5write(1:50, h5file, name = "/group1/dset1")
        fid <- H5Fopen(h5file)
        H5Lcopy(fid, "/group1", fid, "/group2")
        H5Fclose(fid)
    }
    
    h5file <- tempfile()
    createFileWithCopiedGroup(h5file)
    expect_warning(h5ls(h5file), regexp = "Identical objects found")
    
})

############################################################
context("h5ls cleanup")
##########################################################

test_that("No open HDF5 objects are left", {
    expect_equal( length(h5validObjects()), 0 )
})

