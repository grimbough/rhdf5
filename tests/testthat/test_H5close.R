library(rhdf5)

h5File <- tempfile(pattern = "H5Close_", fileext = ".h5")
h5createFile(h5File)
h5write(1:10, file = h5File, name = "DS1")

test_that("Object handles can be closed", {
  ## open file and create orphan group
  fid <- H5Fopen( h5File )
  gid <- H5Gcreate_anon(fid)
  did <- H5Dopen(fid, name = "DS1")
  
  expect_length(h5validObjects(), 3)
  
  ## close only the dataset
  expect_silent(h5closeAll(did))
  expect_length(h5validObjects(), 2)
  
  ## close the file and group ids
  expect_silent(h5closeAll(gid, fid))
  expect_length(h5validObjects(), 0)
  
})


