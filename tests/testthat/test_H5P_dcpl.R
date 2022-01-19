library(rhdf5)

dcpl <- H5Pcreate(type = "H5P_DATASET_CREATE")

test_that("Filters can be set", {
  
  expect_gte(H5Pset_nbit(dcpl), 0)
  expect_gte(H5Pset_shuffle(dcpl), 0)
  
  expect_is( H5Pget_nfilters(dcpl), "integer" ) |>
    expect_equal(2L)
  
  ## we can only set szip for writing with Windows versions built after switching to ucrt
  if( (.Platform$OS.type != "windows") || 
     (!is.null(R.version$crt) && R.version$crt == "ucrt") ) {
    expect_gte(H5Pset_szip(dcpl, options_mask = 1L, pixels_per_block = 8L), 0)
    expect_equal(H5Pget_nfilters(dcpl), 3L)
  }

  
})

test_that("Filter information can be retrieved", {
  
  expect_is(filter_info <- H5Pget_filter(dcpl, 1L), "list") |>
    expect_length(2L)
  expect_equal(filter_info[[2]], "nbit")
  
})

H5Pclose(dcpl)