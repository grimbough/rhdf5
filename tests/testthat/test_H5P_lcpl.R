library(rhdf5)

test_that("Link creation property list can be created", {
    expect_silent(pid <- H5Pcreate(type = "H5P_LINK_CREATE"))
    expect_is(pid , "H5IdComponent")
    expect_silent(H5Pclose(pid))
})    

test_that("Creation of intermediate groups can be toggled", {
  
    pid <- H5Pcreate(type = "H5P_LINK_CREATE")
    on.exit(H5Pclose(pid))
    
    expect_silent(H5Pset_create_intermediate_group( pid, create_groups = TRUE ))
    expect_true(H5Pget_create_intermediate_group( pid ))
    expect_silent(H5Pset_create_intermediate_group( pid, create_groups = FALSE ))
    expect_false(H5Pget_create_intermediate_group( pid ))
    
})   

test_that("LPCL function error handling works", {
  
  pid <- H5Pcreate(type = "H5P_LINK_CREATE")
  on.exit(H5Pclose(pid))
  
  expect_error(H5Pset_create_intermediate_group( pid, create_groups = 1 ), 
               regexp = "The 'create_groups' argument should be either TRUE or FALSE")

})   

test_that("No open HDF5 objects are left", {
    expect_equal( length(h5validObjects()), 0 )
})
