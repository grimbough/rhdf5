library(rhdf5)

h5File <- tempfile(pattern = "H5L_", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

expect_true( h5createFile(h5File) )
expect_silent( h5write(matrix(1:20, ncol = 2), file = h5File, name = "foo") )

############################################################
context("H5L: Check existance")
############################################################

test_that("Links exist", {
    expect_silent( fid <- H5Fopen(h5File) )
    
    expect_true( H5Lexists(fid, "foo") )
    expect_false( H5Lexists(fid, "baa") )
    expect_error( H5Lexists(fid, 1) )
    
    expect_is( H5Lget_info(fid, "foo"), "list" ) %>%
        expect_length(n = 4) %>%
        expect_named(expected = c("type", "corder_valid", "corder", "cset"))
    
    ## this doesn't produce a nice error
    ## H5Lget_info(fid, "baa")
    
    expect_silent( H5Fclose(fid) )
})

############################################################
context("H5Lcreate_external")
############################################################

h5File2 <- tempfile(pattern = "H5L_2_", fileext = ".h5")
if(file.exists(h5File2))
    file.remove(h5File2)

test_that("Creating links between files", {
    expect_true( h5createFile(h5File2) )

    expect_silent( fid <- H5Fopen(h5File2) )
    expect_silent( H5Lcreate_external(target_file_name = h5File, target_obj_name = "/foo", 
                                      link_loc = fid, link_name = "external_link") )
    expect_silent( H5Fclose(fid) )
    
    expect_is( h5read(file = h5File2, name = "external_link"), "matrix" ) %>%
        expect_length(20)
})

############################################################
context("H5Ldelete")
############################################################

test_that("Deleting links", {
    expect_silent( fid <- H5Fopen(h5File) )
    expect_error( H5Ldelete(fid, name = "not_here"), regexp = "link doesn't exist" )
    
    expect_error( H5Ldelete(fid, name = 1L ), 
                  regexp = "'name' must be a character string of length 1" )
    expect_error( H5Ldelete(fid, name = c("foo", "baa")), 
                  regexp = "'name' must be a character string of length 1" )
    
    ## delete 'foo'
    expect_silent( H5Ldelete(fid, "/foo") )
    expect_silent( H5Fclose(fid) )
    
    ## check there are now no entries left
    expect_is( res <- h5ls(h5File), "data.frame" )
    expect_equal( nrow(res), 0 )
})

############################################################
context("H5L cleanup")
##########################################################

test_that("No open HDF5 objects are left", {
    expect_equal( length(h5validObjects()), 0 )
})

