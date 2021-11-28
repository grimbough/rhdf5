library(rhdf5)

tid <- H5Tcopy("H5T_C_S1")

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



test_that("No open HDF5 objects are left", {
    expect_equal( length(h5validObjects()), 0 )
})

