library(rhdf5)

## output file name
h5File <- tempfile(pattern = "ex_read_attr_", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

h5createFile(file = h5File)
A = 1:10
h5write(A, h5File, "A")
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
    
})
