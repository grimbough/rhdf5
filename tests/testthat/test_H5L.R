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

test_that("links can be created between files", {
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

test_that("links can be deleted", {
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
context("Moving Links: H5Lmove")
############################################################

test_that("links can be moved within a file at the same level", {
    expect_silent( h5write(matrix(1:100, ncol = 10), file = h5File, name = "pos1") )
    expect_equal( h5ls(h5File)$name, "pos1" )
    expect_silent( fid <- H5Fopen(h5File) )
    expect_silent( H5Lmove(h5loc = fid, name = "pos1", h5loc_dest = fid, name_dest = "pos2") )
    expect_silent( H5Fclose(fid) )
    expect_equal( h5ls(h5File)$name, "pos2" )
})


test_that("links can be moved to a new group", {
    expect_silent( fid <- H5Fopen(h5File) )
    ## this fails if the group doesn't exist
    expect_error( H5Lmove(h5loc = fid, name = "pos2", h5loc_dest = fid, name_dest = "/foo/pos2") )
    ## create the group and it works
    expect_silent(gid <- H5Gcreate(fid, name = "/foo"))
    expect_silent( H5Lmove(h5loc = fid, name = "pos2", h5loc_dest = fid, name_dest = "/foo/pos2") )
    expect_silent( H5Gclose(gid) )
    expect_silent( H5Fclose(fid) )
    expect_equal( h5ls(h5File)$name, c("foo", "pos2") )
    
    ## we can also pass group ids
    fid <- H5Fopen(h5File)
    gid <- H5Gopen(fid, name = "/foo")
    expect_silent( H5Lmove(h5loc = gid, name = "pos2", h5loc_dest = gid, name_dest = "pos3") )
    H5Gclose(gid)
    H5Fclose(fid)
    expect_equal( h5ls(h5File)$name, c("foo", "pos3") )
})


############################################################
context("Copy Links: H5Lcopy")
############################################################

test_that("links can be copied", {
    expect_silent( fid <- H5Fopen(h5File) )
    expect_silent( H5Lcopy(h5loc = fid, name = "/foo/pos3", h5loc_dest = fid, name_dest = "/foo/pos4") )
    expect_silent( H5Lcopy(h5loc = fid, name = "/foo/pos3", h5loc_dest = fid, name_dest = "pos5") )
    H5Fclose(fid)
    contents <- h5ls(h5File)
    expect_equal(contents$group, c("/", "/foo", "/foo", "/"))
    expect_equal(contents$name, c("foo", "pos3", "pos4", "pos5"))
    expect_equal( h5read(h5File, "/foo/pos3"), h5read(h5File, "pos5") )
})


############################################################
context("H5L cleanup")
##########################################################

test_that("no open HDF5 objects are left", {
    expect_equal( length(h5validObjects()), 0 )
})

