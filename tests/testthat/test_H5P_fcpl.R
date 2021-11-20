library(rhdf5)

test_that("File creation property list can be created", {
    expect_silent(pid <- H5Pcreate(type = "H5P_FILE_CREATE"))
    expect_is(pid , "H5IdComponent")
    expect_silent(H5Pclose(pid))
})    


pid <- H5Pcreate(type = "H5P_FILE_CREATE")

test_that("Sizes of symbol table tree can be retrieved", {
    
    expect_silent(sym_k <- H5Pget_sym_k(pid))
    expect_is(sym_k, "integer")
    expect_identical(names(sym_k), c("ik", "lk"))
    
})   

test_that("Sizes of symbol table tree can be set", {
    
    expect_silent(H5Pset_sym_k(pid, ik = 20L, lk = 5))
    sym_k <- H5Pget_sym_k(pid)
    expect_equivalent(sym_k, c(20L, 5L))
    
}) 

test_that("Sizes of offsets and lengths can be retrieved", {
    
    expect_silent(h5_sizes <- H5Pget_sizes(pid))
    expect_is(h5_sizes, "integer")
    expect_identical(names(h5_sizes), c("offset", "length"))
    
})   

test_that("Sizes of offsets and lengths can be set", {
    
    expect_silent(H5Pset_sizes(pid, sizeof_addr = 16L, sizeof_size = 16L))
    h5_sizes <- H5Pget_sizes(pid)
    expect_equivalent(h5_sizes, c(16L, 16L))
    
}) 

test_that("Size of userblock can be retrieved", {
    
    expect_silent(userblock <- H5Pget_userblock(pid))
    expect_is(userblock, "integer")
    expect_identical(userblock, 0L)

})   

test_that("Size of userblock can be set", {
    
    expect_silent(H5Pset_userblock(pid, size = 512L))
    userblock <- H5Pget_userblock(pid)
    expect_identical(userblock, 512L)
    
    ## valid values are powers of 2 greater than or equal to 512
    #expect_error(H5Pset_userblock(pid, size = 511L))
    
}) 

test_that("Sizes of istore can be retrieved", {
    
    expect_silent(istore <- H5Pget_istore_k(pid))
    expect_is(istore, "integer")
    expect_identical(istore, 32L)
    
})   

test_that("Sizes of istore can be set", {
    
    expect_silent(H5Pset_istore_k(pid, ik = 64L))
    istore <- H5Pget_istore_k(pid)
    expect_equivalent(istore, 64L)
    
    ## max value is 2^16
    #expect_error(H5Pset_userblock(pid, size = 66000))
}) 

test_that("Phase change information can be retrieved", {
    
    expect_silent(p_change <- H5Pget_shared_mesg_phase_change(pid))
    expect_is(p_change, "integer")
    expect_equivalent(p_change, c(50L, 40L))
    
})  

test_that("Phase change information can be set", {
    
    expect_silent(H5Pset_shared_mesg_phase_change(pid, max_list = 25L, min_btree = 20L))
    p_change <- H5Pget_shared_mesg_phase_change(pid)
    expect_equivalent(p_change, c(25L, 20L))
    
}) 

test_that("Number of object header message indexes can be retrieved", {
    
    expect_silent(nindexes <- H5Pget_shared_mesg_nindexes(pid))
    expect_is(nindexes, "integer")
    expect_identical(nindexes, 0L)
    
})   

test_that("Number of object header message indexes can be set", {
    
    expect_silent(H5Pset_shared_mesg_nindexes(pid, nindexes = 8L))
    nindexes <- H5Pget_shared_mesg_nindexes(pid)
    expect_equivalent(nindexes, 8L)
    
}) 

test_that("shared object header mesage index properties can be retrieved", {
    
    ## this can only be run after H5Pset_shared_mesg_nindexes() has been called
    expect_silent(shd_msg_idx <- H5Pget_shared_mesg_index(pid, index_num = 0L))
    expect_is(shd_msg_idx, "list")
    expect_identical(names(shd_msg_idx), c("type_flags", "size"))
    expect_equivalent(shd_msg_idx$type_flags, "H5O_SHMESG_NONE_FLAG")
    expect_equivalent(shd_msg_idx$size, 250)
    
}) 

test_that("shared object header mesage index properties can be set", {
    
    expect_silent(H5Pset_shared_mesg_index(pid, index_num = 0L, 
                                           mesg_type_flags = "H5O_SHMESG_FILL_FLAG", 10L))
    shd_msg_idx <- H5Pget_shared_mesg_index(pid, index_num = 0L)
    expect_equivalent(shd_msg_idx$type_flags, "H5O_SHMESG_FILL_FLAG")
    expect_equivalent(shd_msg_idx$size, 10L)
    
}) 


H5Pclose(pid)

test_that("No open HDF5 objects are left", {
    expect_equal( length(h5validObjects()), 0 )
})
