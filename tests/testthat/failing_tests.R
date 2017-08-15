## Collection of tests that have been written with the expectation that they 
## work or produce useful error messages, but at the moment are not handled
## in a graceful fashion.  Updating the the package code to get these to pass
## is not a priority, but will improve the user experience IMHO

############################################################
context("h5write")
############################################################

D <- 1:20
h5createFile(h5File)
h5createDataset(file = h5File, dataset = "foo", dims = c(2,length(D) ))

expect_message( h5write(obj = D, file = h5File, name = "foo"),
                "dimensions of 'obj' and the 'foo' dataset are different" )

############################################################
context("h5set_extent")
############################################################

h5File <- tempfile(pattern = "ex_set_extent_", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)
D <- 1:20
h5createFile(h5File)
h5createDataset(file = h5File, dataset = "foo", dims = c(1,length(D)), maxdims = c(2,length(D)))
h5write(obj = D, file = h5File, name = "foo")

## you can't pass a dataset ID to this function, you always get the 
## "file open" warning, but you can't close it before reading.
test_that("Work with dataset identifier", {
    fid <- H5Fopen(h5File)
    did <- H5Dopen(fid, name = "foo")
    H5Fclose(fid)
    
    expect_equal( h5set_extent(file = h5File, dataset = did, dims = c(1, length(D))), 0 )

    H5Dclose(did)
})