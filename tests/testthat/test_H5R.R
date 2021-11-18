library(rhdf5)

h5File <- tempfile(pattern = "ex_H5R_", fileext = ".h5")
h5createFile(h5File)
h5createGroup(file = h5File, group = "/foo")
h5write(1:100, file=h5File, name="/foo/baa")

fid <- H5Fopen(h5File)

test_that("object references can be created", {
  
  expect_silent(ref_to_group <- H5Rcreate(fid, name = "/foo")) 
  expect_silent(ref_to_dataset <- H5Rcreate(fid, name = "/foo/baa"))
  
  expect_is(ref_to_group, "H5Ref")
  expect_is(ref_to_dataset, "H5Ref")
  
  expect_equal(H5Rget_name(ref = ref_to_group, h5loc = fid),
               "/foo")
  expect_equal(H5Rget_name(ref = ref_to_dataset, h5loc = fid),
               "/foo/baa")
  
  expect_equal(H5Rget_obj_type(ref = ref_to_group, h5loc = fid),
               "GROUP")
  expect_equal(H5Rget_obj_type(ref = ref_to_dataset, h5loc = fid),
               "DATASET")
  
  expect_silent(gid <- H5Rdereference(ref = ref_to_group, h5loc = fid)) |>
    expect_is("H5IdComponent")
  expect_silent(did <- H5Rdereference(ref = ref_to_dataset, h5loc = fid)) |>
    expect_is("H5IdComponent")
  
  H5Gclose(gid)
  H5Dclose(did)
})


## get the dataspace for /foo/baa and select three points
did <- H5Dopen(fid, name = "/foo/baa")
h5space <- H5Dget_space(did)
H5Sselect_index(h5space = h5space, c(4,7,50))
H5Dclose(did)

test_that("dataset region references can be created", {
  
  expect_silent(ref_to_region <- H5Rcreate(fid, name = "/foo/baa", ref_type = "H5R_DATASET_REGION", h5space = h5space))
  
  expect_is(ref_to_region, "H5Ref")
  
  expect_equal(H5Rget_name(ref = ref_to_region, h5loc = fid),
               "/foo/baa")
  
  expect_silent(sid <- H5Rget_region(ref = ref_to_region, h5loc = fid)) |>
    expect_is('H5IdComponent')
  expect_equal(H5Sget_select_npoints(sid), 3L)
  
  expect_equal(H5Rget_obj_type(ref = ref_to_region, h5loc = fid),
               "DATASET")
  
  expect_silent(did <- H5Rdereference(ref = ref_to_region, h5loc = fid)) |>
    expect_is("H5IdComponent")

  H5Sclose(sid)
  H5Dclose(did)
})

H5Sclose(h5space)
H5Fclose(fid)


test_that("No open HDF5 objects are left", {
  expect_equal( length(h5validObjects()), 0 )
})
