test_that("H5Tclose works for enum datatypes", {
  tid <- H5Tenum_create(dtype_id = "H5T_NATIVE_UCHAR")
  H5Tenum_insert(tid, name = "TRUE", value = 1L)
  H5Tenum_insert(tid, name = "FALSE", value = 0L)

  # Should be able to close the datatype
  expect_identical(H5Tclose(tid), 0L)
})

test_that("H5Tclose works for copied datatypes", {
  tid <- H5Tcopy("H5T_C_S1")
  H5Tset_size(tid, 10)

  # Should be able to close the datatype
  expect_identical(H5Tclose(tid), 0L)
})
