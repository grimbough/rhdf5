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

test_that("H5R error checking works", {
    
    expect_error(H5Rcreate(fid, name = "/foo/baa", ref_type = "H5R_DATASET_REGION"),
                 regexp = "H5R_DATASET_REGION references must be accompanied by a H5 dataspace")
    
    ref_to_dataset <- H5Rcreate(fid, name = "/foo/baa")
    expect_error(H5Rget_region(ref = ref_to_dataset, h5loc = fid),
                 "Only references of type H5R_DATASET_REGION can be used")
    
})

test_that("H5Ref methods work", {
    
    object_ref <- H5Rcreate(fid, name = "/foo/baa", ref_type = "H5R_OBJECT")
    region_ref <- H5Rcreate(fid, name = "/foo/baa", ref_type = "H5R_DATASET_REGION", h5space = h5space)

    expect_silent(object_ref2 <- c(object_ref, object_ref)) |>
        expect_is("H5Ref")
    expect_silent(region_ref2 <- c(region_ref, region_ref)) |>
        expect_is("H5Ref")
    
    expect_error(c(object_ref, region_ref),
                 "All references must be of the same type")
    expect_error(c(object_ref, 1:10),
                 "All objects must be of class 'H5Ref'")
    
    ## object and region references are different internally but not externally
    expect_equal(length(object_ref), 1)
    expect_equal(length(object_ref2), 2)
    expect_equal(length(region_ref), 1)
    expect_equal(length(region_ref2), 2)
    
    expect_equal(length(object_ref[2]), 1)
    expect_equivalent(object_ref[1], object_ref[2])
})

H5Sclose(h5space)
H5Fclose(fid)


test_that("No open HDF5 objects are left", {
  expect_equal( length(h5validObjects()), 0 )
})
