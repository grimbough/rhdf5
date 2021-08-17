library(rhdf5)

## output file name
h5File <- tempfile(pattern = "ex_read_attr_", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

h5createFile(file = h5File)
A <- as.double(1:10)
h5write(A, h5File, "A", )
fid <- H5Fopen(h5File) 
did <- H5Dopen(fid, "A") 
sid <- H5Screate_simple(10)

## 64-bit integers
aid <- H5Acreate(did, "int64", "H5T_NATIVE_INT64", sid) 
H5Awrite(aid, c(1:9, 2^32))
H5Aclose(aid) 

## unsigned 32-bit integers
aid <- H5Acreate(did, "uint32", "H5T_NATIVE_UINT32", sid) 
H5Awrite(aid, c(1:9, 2^31)) 
H5Aclose(aid) 

H5Sclose(sid) 
H5Dclose(did) 
H5Fclose(fid) 

test_that('64-bit integer attributes are read correctly', {
    
    expect_warning(x1 <- h5readAttributes(h5File, "A", bit64conversion = "int"))
    expect_true("int64" %in% names(x1) && "uint32" %in% names(x1))
    expect_equivalent(x1$int64, c(1:9, NA))
    expect_equivalent(x1$uint32, c(1:9, NA))
    
    expect_silent(x2 <- h5readAttributes(h5File, "A", bit64conversion = "double"))
    expect_equivalent(x2$int64, c(1:9, 2^32))
    expect_equivalent(x2$uint32, c(1:9, 2^31))
    expect_is(x2$int64, "array")
    expect_type(x2$int64, "double")
    
    expect_silent(x3 <- h5readAttributes(h5File, "A", bit64conversion = "bit64"))
    expect_equivalent(x3$int64, c(1:9, 2^32))
    expect_equivalent(x3$uint32, c(1:9, 2^31))
    expect_s3_class(x3$int64, "integer64")
    
})
