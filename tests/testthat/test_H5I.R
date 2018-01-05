library(rhdf5)

############################################################
context("H5I methods")
############################################################

h5File <- tempfile(pattern = "H5_methods", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

fid <- H5Fcreate(name = h5File)
gid <- H5Gcreate(fid, name = "foo")
sid <- H5Screate_simple(dims = c(5,5)) 
did <- H5Dcreate(h5loc = fid, name = "baa", dtype_id = "H5T_NATIVE_UINT32", h5space = sid) 

test_that("getting names", {
    
    expect_match( H5Iget_name(fid), "/")
    expect_match( H5Iget_name(gid), "/foo")
    expect_match( H5Iget_name(did), "/baa")
    
    expect_error( H5Iget_name(sid), "The provided H5Identifier is not an object identifier")
    expect_error( H5Iget_name("test"), "Argument not of class H5IdComponent" )
})

test_that("getting types", {
    
    expect_match( as.character(H5Iget_type(fid)), "H5I_FILE")
    expect_match( as.character(H5Iget_type(gid)), "H5I_GROUP")
    expect_match( as.character(H5Iget_type(did)), "H5I_DATASET")
    expect_match( as.character(H5Iget_type(sid)), "H5I_DATASPACE")
    
    expect_error( H5Iget_type("test") )
})

test_that("getting types", {
    
    expect_true( H5Iis_valid(fid) )
    expect_true( H5Iis_valid(did) )
    expect_true( H5Iis_valid(gid) )
    expect_true( H5Iis_valid(sid) )
    
    ## try with a closed object
    H5Sclose(sid)
    expect_false( H5Iis_valid(sid) )
    
    expect_error( H5Iis_valid("test") )
    expect_error( H5Iis_valid(1L) )
})

H5Dclose(did)
H5Gclose(gid)
H5Fclose(fid)


