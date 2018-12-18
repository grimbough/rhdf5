library(rhdf5)

############################################################
context("H5P functions")
############################################################

test_that("Property list creation & closure", {
    expect_silent(pid <- H5Pcreate())
    expect_is(pid , "H5IdComponent")
    expect_silent(H5Pclose(pid))
})    


test_that("Property list creation & closure", {
    expect_silent( pid1 <- H5Pcreate() )
    
    expect_silent( pid2 <- H5Pcopy(pid1) )
    expect_is(pid2 , "H5IdComponent")
    
    expect_silent(H5Pclose(pid1))
    expect_silent(H5Pclose(pid2))
})    

test_that("setting and getting libhdf5 version bounds", {
    expect_silent( pid1 <- H5Pcreate("H5P_FILE_ACCESS") )
    
    expect_output( version_bounds <- H5Pget_libver_bounds(pid1), 
                   regexp = "^low" )
    expect_is( version_bounds, "factor" )
    expect_equivalent(version_bounds, 
                      as.factor(c("H5F_LIBVER_EARLIEST","H5F_LIBVER_LATEST")))
    ## V18 is different from both EARLIEST and LATEST
    expect_silent( H5Pset_libver_bounds(pid1, libver_low = "H5F_LIBVER_EARLIEST", libver_high = "H5F_LIBVER_V18") )
    expect_output( version_bounds <- H5Pget_libver_bounds(pid1), 
                   regexp = "^low" )
    expect_equivalent(version_bounds, 
                      as.factor(c("H5F_LIBVER_EARLIEST","H5F_LIBVER_V18")))
    ## V110 is the same as using LATEST
    expect_silent( H5Pset_libver_bounds(pid1, libver_low = "H5F_LIBVER_V110", libver_high = "H5F_LIBVER_LATEST") )
    expect_output( version_bounds <- H5Pget_libver_bounds(pid1), 
                   regexp = "^low" )
    expect_equivalent(version_bounds, 
                      as.factor(c("H5F_LIBVER_LATEST","H5F_LIBVER_LATEST")))
    
    expect_silent(H5Pclose(pid1))
})  

test_that("Dataset creation properties", {
    
    expect_silent(pid <- H5Pcreate("H5P_DATASET_CREATE"))
    
    ## use default layout
    expect_silent( layout <- H5Pget_layout(pid) )
    expect_is( layout, "factor" )
    expect_match( as.character(layout), "H5D_CONTIGUOUS")
    ## change to chunked
    expect_silent( H5Pset_layout(pid, layout = "H5D_CHUNKED") )
    ## check changes
    expect_silent( layout_new <- H5Pget_layout(pid) )
    expect_match( as.character(layout_new), "H5D_CHUNKED")
    
    ## seting chunk sizes
    expect_null( H5Pget_chunk( pid ) )
    H5Pset_chunk( pid, dim = c(100,100) )
    expect_equal( H5Pget_chunk(pid), c(100,100) )
    
    ## default fill values
    ## I think this is defined by default
    expect_true( H5Pfill_value_defined(pid) )
    expect_silent( H5Pset_fill_value( pid, 10 ) )
    expect_silent( H5Pset_fill_value( pid, 10L ) )
    expect_silent( H5Pset_fill_value( pid, "foo" ) )
    expect_silent( H5Pset_fill_value( pid, TRUE ) )
    expect_error( H5Pset_fill_value( pid, sum ))
    
    expect_silent(H5Pclose(pid))
})

test_that("No open HDF5 objects are left", {
    expect_equal( length(h5validObjects()), 0 )
})
