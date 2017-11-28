library(rhdf5)

############################################################
context("H5Pcreate")
############################################################

test_that("Property list creation & closure", {
    expect_silent(pid <- H5Pcreate())
    expect_is(pid , "H5IdComponent")
    expect_silent(H5Pclose(pid))
})    


############################################################
context("H5P copying")
############################################################

test_that("Property list creation & closure", {
    expect_silent( pid1 <- H5Pcreate() )
    
    expect_silent( pid2 <- H5Pcopy(pid1) )
    expect_is(pid , "H5IdComponent")
    
    expect_silent(H5Pclose(pid1))
    expect_silent(H5Pclose(pid2))
})    

############################################################
context("H5P version bounds")
############################################################

test_that("setting and getting libhdf5 version bounds", {
    expect_silent( pid1 <- H5Pcreate("H5P_FILE_ACCESS") )
    
    expect_output( version_bounds <- H5Pget_libver_bounds(pid1), 
                   regexp = "^low" )
    expect_is( version_bounds, "factor" )
    expect_equivalent(version_bounds, 
                      as.factor(c("H5F_LIBVER_EARLIEST","H5F_LIBVER_18")))
    
    expect_silent( H5Pset_libver_bounds(pid1, libver_low = "H5F_LIBVER_18", libver_high = "H5F_LIBVER_LATEST") )
    expect_output( version_bounds <- H5Pget_libver_bounds(pid1), 
                   regexp = "^low" )
    expect_equivalent(version_bounds, 
                      as.factor(c("H5F_LIBVER_18","H5F_LIBVER_18")))
    
    expect_silent(H5Pclose(pid1))
})  