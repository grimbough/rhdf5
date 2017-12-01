library(rhdf5)

############################################################
context("Print methods")
############################################################

h5File <- tempfile(pattern = "H5_methods", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

test_that("Printing various object types", {
    
    ## file
    expect_silent(fid <- H5Fcreate(name = h5File))
    expect_output(print(fid), regexp = "HDF5 FILE")
    
    ## group
    expect_silent( gid <- H5Gcreate(fid, name = "foo") ) 
    expect_output(print(gid), regexp = "HDF5 GROUP")
    expect_silent(H5Gclose(gid))
    
    ## dataspace
    expect_silent( sid <- H5Screate_simple(dims = c(5,5)) )
    expect_output( print(sid), regexp = "HDF5 DATASPACE" )
    
    ## dataset
    expect_silent( did <- H5Dcreate(h5loc = fid, name = "baa", dtype_id = "H5T_NATIVE_UINT32", h5space = sid) )
    expect_output( print(did), regexp = "HDF5 DATASET" )
    
    ## datatype
    expect_silent( tid <- H5Tcopy("H5T_NATIVE_INT32") )
    expect_output( print(tid), regexp = "[0-9]{8}")

    ## attribute
    expect_silent( aid <- H5Acreate(did, name = "bang", dtype_id = tid, h5space = sid) )
    expect_output( print(aid), regexp = "HDF5 ATTR" )
    
    ## close everything
    expect_silent( H5Aclose(aid) )
    expect_silent( H5Sclose(sid) )
    expect_silent( H5Dclose(did) )
    expect_silent( H5Fclose(fid) )
})

############################################################
context("Subsetting methods")
############################################################

h5File <- tempfile(pattern = "H5_methods", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

test_that("Subsetting datasets", {
    
    A <- matrix(data = 1:200, nrow = 10)
    h5save(A, file = h5File)
    
    expect_silent(fid <- H5Fopen(name = h5File))
    expect_silent(did <- H5Dopen(h5loc = fid, name = "A"))
    
    expect_silent( col1 <- did[,1] )
    expect_is( col1, "integer" )
    
    expect_silent( col15 <- did[,1:5] )
    expect_is( col15, "matrix" )
    expect_equal( dim(col15), c(10,5) )
    expect_silent( H5Dclose(did) )
    
    expect_error( did[], 
                  regexp = "Bad HDF5 ID")
    expect_error( fid[,3], 
                  regexp = "The provided H5Identifier is not a dataset identifier" )
    
    expect_silent( H5Fclose(fid) )
})

test_that("Subsetting assignment", {
    
    expect_silent(fid <- H5Fopen(name = h5File))
    expect_silent(did <- H5Dopen(h5loc = fid, name = "A"))
    
    ## assign new values
    expect_silent( did[10,] <- did[10,] + 1000 )
    expect_silent( did[,1] <- 1001:1010 )
    expect_silent( did[1:3,5:7] <- rep(0,9) )
    ## in native R the 0 would be repeated to fill the space
    # expect_silent( did[1:3,5:7] <- 0 )

    ## close dataset and check the values are permanent
    expect_silent( H5Dclose(did) )
    expect_silent( did <- H5Dopen(h5loc = fid, name = "A") )
    expect_equal( did[,1], 1001:1010)
    expect_equal( did[10,], seq(10,200,10) + 1000 )
    expect_equal( did[1:3,5:7], matrix(0, ncol = 3, nrow = 3) )
    expect_silent( H5Dclose(did) )
    
    expect_error( did[,1] <- 10:1, 
                  regexp = "Bad HDF5 ID")
    expect_error( fid[,3] <- 10, 
                  regexp = "The provided H5Identifier is not a dataset identifier" )

    expect_silent( H5Fclose(fid) )
})

############################################################
context("Methods cleanup")
##########################################################

test_that("No open HDF5 objects are left", {
    expect_equal( length(h5validObjects()), 0 )
})
