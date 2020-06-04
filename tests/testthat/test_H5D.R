library(rhdf5)

h5File <- tempfile(pattern = "H5D_", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

expect_true( h5createFile(h5File) )
expect_silent( h5write(matrix(1:20, ncol = 2), file = h5File, name = "foo") )

############################################################
context("H5D: getting property lists")
###########################################################

## The property list interface is really limited at the moment
## so there aren't many functions that we can check
test_that("we can extract the property lists", {
    expect_silent( fid <- H5Fopen(h5File) )
    expect_silent( did <- H5Dopen(fid, name = "foo") )
    
    expect_silent( pid <- H5Dget_create_plist(did) )
    expect_output( print(pid), "HDF5 GENPROP_LST")
    
    expect_silent( H5Pclose(pid) )
    expect_silent( H5Dclose(did) )
    expect_silent( H5Fclose(fid) )
})
