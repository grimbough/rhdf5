library(rhdf5)

## output file name
h5File <- tempfile(pattern = "ex_save", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

############################################################
context("H5Gcreate")
############################################################

test_that("Group creation & closure", {
    
    expect_silent(fid <- H5Fcreate(name = h5File))
    ## writing to a group
    expect_silent(gid <- H5Gcreate(h5loc = fid, name = "test_group"))
    expect_silent( h5write(matrix(1:20, ncol = 10), file = gid, name = "foo") )

    expect_silent(H5Gclose(gid))
    expect_silent(H5Fclose(fid))
})

############################################################
context("H5G info")
############################################################

test_that("Group creation & closure", {
    
    expect_silent(fid <- H5Fopen(name = h5File))
    ## writing to a group
    expect_silent(gid <- H5Gopen(h5loc = fid, name = "test_group"))
    expect_silent( info <- H5Gget_info(gid) )
    expect_is( info, "list" )
    expect_named( info, c("storage_type", "nlink", "max_corder", "mounted") )
    
    expect_silent(H5Gclose(gid))
    ## we can use the name rather than the group_id
    expect_silent( info2 <- H5Gget_info_by_name(fid, "test_group") )
    expect_identical( info, info2 )
    
    ##also by index
    expect_silent( info3 <- H5Gget_info_by_idx(fid, n = 1) )
    expect_identical( info, info3 )
    
    expect_silent(H5Fclose(fid))
})

############################################################
context("H5S cleanup")
##########################################################

test_that("No open HDF5 objects are left", {
    expect_equal( length(h5validObjects()), 0 )
})