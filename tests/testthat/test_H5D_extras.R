library(rhdf5)

h5File <- tempfile(pattern = "H5D_extras_", fileext = ".h5")
h5createFile(h5File)

test_that("we can determine dataset chunk properties", {

  fid <- H5Fopen(h5File)
  sid <- H5Screate_simple(c(100, 100))
  dcpl <- H5Pcreate("H5P_DATASET_CREATE")
  H5Pset_layout(dcpl, layout = "H5D_CONTIGUOUS")
  did1 <- H5Dcreate(fid, name = "/not_chunked", h5space = sid,
                    dtype_id = "H5T_NATIVE_INT", dcpl = dcpl)
  H5Pset_layout(dcpl, layout = "H5D_CHUNKED")
  H5Pset_chunk(dcpl, dim = c(5, 20))
  did2 <- H5Dcreate(fid, name = "/chunked", h5space = sid,  
                    dtype_id = "H5T_NATIVE_INT", dcpl = dcpl)
  
  on.exit(h5closeAll(fid, sid, dcpl, did1, did2))
  
  expect_false(H5Dis_chunked(did1))
  expect_true(H5Dis_chunked(did2))
  
  expect_null(H5Dchunk_dims(did1))
  expect_identical(H5Dchunk_dims(did2), c(5L,20L))
})



