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

test_that("UTF8 strings can be used for fill values", {
  
  fill_value <- "αααα-test"
  
  tf <- tempfile(fileext = ".h5")
  fid <- H5Fcreate(tf)
  sid <- H5Screate_simple(dims = 1)
  tid <- H5Tcopy("H5T_C_S1")
  H5Tset_size(tid, size = nchar(fill_value, type = "bytes")+1)
  H5Tset_cset(tid, "UTF-8")
  
  pid <- H5Pcreate(type = "H5P_DATASET_CREATE")
  expect_silent(H5Pset_fill_value(pid, fill_value))
  
  did1 <- H5Dcreate(h5loc = fid, name = "/strings", dtype_id = tid, dcpl = pid, h5space = sid)

  H5Dclose(did1)
  H5Pclose(pid)
  H5Sclose(sid)
  H5Fclose(fid)
  
  expect_equivalent(h5read(tf, name = "/strings"), fill_value)
  
})

test_that("H5P error handling works", {
  
  expect_silent(pid <- H5Pcreate("H5P_DATASET_CREATE"))
  
  expect_error(H5Pset_obj_track_times(pid, track_times = "TEST"))
  expect_error(H5Pset_obj_track_times(pid, track_times = 1L))
  expect_error(H5Pset_obj_track_times(pid, track_times = NA))
  
  H5Pclose(pid)
})

H5Pclose(dcpl)