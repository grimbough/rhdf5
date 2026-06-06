test_that("H5Dchunk_iter rejects non-chunked datasets", {
  h5file <- system.file("testfiles", "h5ex_t_array.h5", package = "rhdf5")
  fid <- H5Fopen(h5file, flags = "H5F_ACC_RDONLY")
  did <- H5Dopen(fid, "DS1")
  on.exit({ H5Dclose(did); H5Fclose(fid) })
  expect_error(H5Dchunk_iter(did))
})

test_that("H5Dchunk_iter returns correct structure for chunked dataset", {
  h5file <- system.file("testfiles", "h5ex_d_szip.h5", package = "rhdf5")
  fid <- H5Fopen(h5file, flags = "H5F_ACC_RDONLY")
  did <- H5Dopen(fid, "DS1")
  on.exit({ H5Dclose(did); H5Fclose(fid) })
  
  result <- H5Dchunk_iter(did)
  
  expect_type(result, "list")
  expect_named(result, c("offset", "filter_mask", "addr", "size"))
})

test_that("H5Dchunk_iter offset matrix has correct shape and values", {
  h5file <- system.file("testfiles", "h5ex_d_szip.h5", package = "rhdf5")
  fid <- H5Fopen(h5file, flags = "H5F_ACC_RDONLY")
  did <- H5Dopen(fid, "DS1")
  on.exit({ H5Dclose(did); H5Fclose(fid) })
  
  result <- H5Dchunk_iter(did)
  
  expect_true(is.matrix(result$offset))
  expect_equal(ncol(result$offset), 2L)   # 2D dataset
  expect_equal(nrow(result$offset), 64L)  # 64 chunks in h5ex_d_szip DS1
  
  # first chunk is at origin
  expect_equal(result$offset[1, ], c(0, 0))
})

test_that("H5Dchunk_iter addr and size are positive numeric vectors", {
  h5file <- system.file("testfiles", "h5ex_d_szip.h5", package = "rhdf5")
  fid <- H5Fopen(h5file, flags = "H5F_ACC_RDONLY")
  did <- H5Dopen(fid, "DS1")
  on.exit({ H5Dclose(did); H5Fclose(fid) })
  
  result <- H5Dchunk_iter(did)
  n <- nrow(result$offset)
  
  expect_length(result$addr, n)
  expect_length(result$size, n)
  expect_length(result$filter_mask, n)
  
  expect_true(all(result$addr > 0))
  expect_true(all(result$size > 0))
})

test_that("H5Dchunk_iter requires an H5IdComponent dataset", {
  expect_error(H5Dchunk_iter("not_a_dataset"))
  expect_error(H5Dchunk_iter(42L))
})