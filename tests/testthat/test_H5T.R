library(rhdf5)

tid <- H5Tcopy("H5T_C_S1")
integer_tid <- H5Tcopy("H5T_STD_U32LE")

test_that("String padding can be read and changed", {
    
    expect_silent(tid2 <- H5Tset_strpad(dtype_id = tid,  strpad = "NULLTERM"))
    expect_equal(H5Tget_strpad(tid), 0L)
    expect_silent(tid2 <- H5Tset_strpad(dtype_id = tid,  strpad = "NULLPAD"))
    expect_equal(H5Tget_strpad(tid), 1L)
    expect_silent(tid2 <- H5Tset_strpad(dtype_id = tid,  strpad = "SPACEPAD"))
    expect_equal(H5Tget_strpad(tid), 2L)
})

test_that("String character set can be read and changed", {

  expect_equal(H5Tget_cset(tid), 0L)
  
  expect_silent(H5Tset_cset(tid, cset = "UTF-8")) |>
    expect_gte(0)
  
  expect_equal(H5Tget_cset(tid),1L)
  
})

test_that("H5T error handling works", {
    expect_error(H5Tget_strpad())
    expect_error(H5Tset_strpad(dtype_id = tid, strpad = "FOOBAA"))
    
    expect_error(H5Tget_size())
    expect_error(H5Tset_size(dtype_id = "H5T_C_S1"))
    
    expect_error(H5Tget_cset())
    expect_error(H5Tget_cset(dtype_id = tid, cset = "FOOBAA"))
})

test_that("Precision can be modified", {
  expect_identical(H5Tget_precision(integer_tid), 32L)
  expect_true(H5Tset_precision(integer_tid, precision = 8))
  expect_identical(H5Tget_precision(integer_tid), 8L)
  
  expect_error(H5Tget_precision(), regexp = "Argument 'dtype_id' must be supplied")
  expect_error(H5Tset_precision(), regexp = "Argument 'dtype_id' must be supplied")
  expect_error(H5Tset_precision(integer_tid, 0), regexp = "'precision' argument must be greater than 0")
})

test_that("Enum datatypes can be created and modified", {
  expect_silent(tid <- H5Tenum_create(dtype_id = "H5T_NATIVE_UCHAR"))
  expect_is(tid, "character")
  
  expect_true(H5Tenum_insert(tid, name = "TRUE", value = 1L))
  expect_true(H5Tenum_insert(tid, name = "FALSE", value = 0L))
})

test_that("No open HDF5 objects are left", {
    expect_equal( length(h5validObjects()), 0 )
})

