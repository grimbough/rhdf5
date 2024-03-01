library(rhdf5)

############################################################
context("H5Screate")
############################################################

test_that("Dataspace creation & closure", {
    expect_silent(sid <- H5Screate())
    expect_is(sid , "H5IdComponent")
    expect_silent(H5Sclose(sid))
    
    expect_silent(sid <- H5Screate(type = "H5S_SIMPLE"))
    expect_is(sid , "H5IdComponent")
    expect_silent(H5Sclose(sid))
    
    expect_silent(sid <- H5Screate(type = "H5S_NULL"))
    expect_is(sid , "H5IdComponent")
    expect_silent(H5Sclose(sid))
    
    expect_null( h5errorHandling(type = "suppress") )  
    expect_message( sid <- H5Screate(type = "FOO") )
    expect_false(sid)
})
 
############################################################
context("H5Scopy")
############################################################

test_that("Dataspace copying", {
    sid <- H5Screate()
    expect_silent(sid2 <- H5Scopy(sid))
    expect_false( identical(sid, sid2) )
    expect_silent(H5Sclose(sid))
    expect_silent(H5Sclose(sid2))
})

test_that("Dataspace copy fails on bad input", {
    expect_error(sid <- H5Scopy("foo"))
})

############################################################
context("H5S simple")
############################################################

test_that("We can create a simple dataspace", {
    expect_silent(sid <- H5Screate_simple(dims = c(10,2)))
    expect_is(sid , "H5IdComponent")
    expect_true(H5Sis_simple(sid))
    
    expect_is(dspace_dims <- H5Sget_simple_extent_dims(sid), "list")
    expect_named(dspace_dims, c("rank", "size", "maxsize"))
    expect_equal(dspace_dims$size, dspace_dims$maxsize)
    expect_equal(dspace_dims$rank, 2)
    
    ## if maxdims isn't supplied, it just matches dims
    expect_silent(H5Sset_extent_simple(sid, dims = c(1,1,1)))
    dspace_dims <- H5Sget_simple_extent_dims(sid)
    expect_equal(dspace_dims$size, dspace_dims$maxsize)
    
    expect_silent(H5Sset_extent_simple(sid, dims = c(1,2,3), maxdims = c(10,20,30)))
    expect_is(dspace_dims <- H5Sget_simple_extent_dims(sid), "list")
    expect_false( identical(dspace_dims$size, dspace_dims$maxsize) )
    expect_equal(dspace_dims$rank, 3)
    
    expect_silent(H5Sclose(sid))
})

test_that("We can create a dataspace with unlimited or non 32-bit integer dims", {
  expect_silent(sid <- H5Screate_simple(dims = c(10, 2), maxdims = c(10, 3e9)))
  dspace_dims <- H5Sget_simple_extent_dims(sid)
  expect_is(dspace_dims$maxsize, 'numeric')
  expect_equal(dspace_dims$maxsize, c(10, 3e9))
  
  expect_silent(H5Sset_extent_simple(sid, dims = c(1,2), maxdims = c(10, H5Sunlimited())))
  dspace_dims <- H5Sget_simple_extent_dims(sid)
  expect_is(dspace_dims$maxsize, 'integer')
  expect_equal( dspace_dims$maxsize, c(10, -1) )
  
  expect_silent(H5Sclose(sid))
})

############################################################
context("H5S Select Hyperslab")
############################################################

test_that("Selecting hyperslabs", {
    
    expect_silent(sid <- H5Screate_simple(dims = c(10,20)))

    expect_silent( H5Sselect_hyperslab(sid) )

    ## errors when not providing enough dimensions
    expect_error( H5Sselect_hyperslab(sid, start = 2) )
    expect_error( H5Sselect_hyperslab(sid, stride = 2) )
    expect_error( H5Sselect_hyperslab(sid, count = 2) )
    expect_error( H5Sselect_hyperslab(sid, block = 2) )
    
    expect_silent(H5Sclose(sid))
    
    ## error in incorrect input
    expect_error(  H5Sselect_hyperslab("foo") )
})

############################################################
context("H5S Select Index")
############################################################

test_that("Selecting using an index", {
    
    expect_silent(sid <- H5Screate_simple(dims = c(10,20,30)))
    
    expect_silent( size <- H5Sselect_index(sid, index = list(1:5, 1:5, 11:15)) )
    expect_identical( size, c(5,5,5) )
    
    expect_silent( size <- H5Sselect_index(sid, index = list(NULL, NULL, NULL)) )
    expect_identical( size, c(10,20,30) )
    
    ## errors when not providing enough dimensions or incorrect dimensions
    expect_error( H5Sselect_index(sid, index = list(10)),
                  regexp = "length of list index not equal to h5space dimensional extension")
    expect_error( H5Sselect_index(sid, index = list(1:5, 1:5, 0:5)), 
                  regexp = "negative indices and 0 not supported" )
    expect_error( H5Sselect_index(sid, index = list(1:15, 1:5, 1:5)), 
                  regexp = "index exceeds HDF5-array dimension")
    
    expect_silent(H5Sclose(sid))
})

test_that("Other selection functions", {
  
  dims <- c(10,20,30)
  expect_silent(sid <- H5Screate_simple(dims = dims))
  
  expect_silent( H5Sselect_all(sid) )
  expect_identical( H5Sget_select_npoints(sid), prod(dims) )
  
  expect_silent( H5Sselect_none(sid) )
  expect_identical( H5Sget_select_npoints(sid), 0 )
  
  expect_silent(H5Sclose(sid))
})

test_that("Combining selections", {
  
  sid_1 <- H5Screate_simple(dims = 20)
  sid_2 <- H5Screate_simple(dims = 10)

  ## select a single block of 5 points in sid_1
  ## this is equivalent to [11:16] in R syntax
  H5Sselect_hyperslab(sid_1, start = 11, stride = 1,
                      block = 5, count = 1)

  ## select 2 blocks of 1 point from sid_2
  ## equivalent to [c(3,5)] in R syntax
  H5Sselect_hyperslab(sid_2, start = 3, stride = 2,
                      block = 1, count = 2)

  ## confirm we have select 5 and 2 points resepectively
  expect_equal(H5Sget_select_npoints(sid_1), 5)
  expect_equal(H5Sget_select_npoints(sid_2), 2)

  ## combine the two dataset selections keeping points that
  ## are in one or both of the selections
  sid_3 <- H5Scombine_select(sid_1, "H5S_SELECT_OR", sid_2)

  ## extent of the new dataset is the same as sid_1
  sid_3
  ## confirm the selection contains 7 points
  expect_equal(H5Sget_select_npoints(sid_3), 7)

  ## tidy up
  H5Sclose(sid_1)
  H5Sclose(sid_2)
  H5Sclose(sid_3)
  
  
})

############################################################
context("H5S cleanup")
##########################################################

test_that("No open HDF5 objects are left", {
    expect_equal( length(h5validObjects()), 0 )
})

