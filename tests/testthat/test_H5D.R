library(rhdf5)

h5File <- tempfile(pattern = "H5D_", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

expect_true( h5createFile(h5File) )
expect_silent( h5write(matrix(1:200, ncol = 2), file = h5File, name = "foo") )

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

test_that("dataset size is reported", {
    
    expect_silent( fid <- H5Fopen(h5File) )
    expect_silent( did <- H5Dopen(fid, name = "foo") )
    
    expect_silent( dset_size <- H5Dget_storage_size(did) )
    expect_is( dset_size, "integer")
    expect_gt( dset_size, 0 )
    
    expect_silent( H5Dclose(did) )
    expect_silent( H5Fclose(fid) )
    
})

test_that("we can change the size of a dataset", {

  h5createDataset(file = h5File, dataset = "/size_changer", 
                  dims = c(0, 0),  
                  maxdim = c(H5Sunlimited(), 1000),
                  chunk = c(100, 100), 
                  storage.mode = "integer")
  
  fid <- H5Fopen(h5File)
  did <- H5Dopen(fid, name = "/size_changer")
  on.exit(h5closeAll(did, fid))
  
  sid <- H5Dget_space(did)
  expect_equal(H5Sget_simple_extent_dims(sid)$size, c(0,0))
  H5Sclose(sid)
  
  ## change both i and j a few times
  for(i in seq(100, 1000, by = 200)) {
    for(j in seq(0, 1000, by = 500)) {
    expect_true(H5Dset_extent(did, size = c(i, j)))
    sid <- H5Dget_space(did)
    expect_equal(H5Sget_simple_extent_dims(sid)$size, c(i,j))
    H5Sclose(sid)
    }
  }
  
  ## can't set outside of maxdim
  expect_false(H5Dset_extent(did, size = c(2000, 2000)))
  ## size remain where we set it last
  sid <- H5Dget_space(did)
  expect_equal(H5Sget_simple_extent_dims(sid)$size, c(i,j))
  H5Sclose(sid)
  
})
