library(rhdf5)

############################################################
context("H5A functions")
############################################################

h5File <- tempfile(pattern = "ex_H5A_", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

h5createFile(h5File)
h5write(1:15, h5File,"A")

expect_null( h5errorHandling(type = "suppress") )  

test_that("Writing attributes", {
    expect_silent(fid <- H5Fopen(h5File) )
    expect_silent(did <- H5Dopen(fid, "A") )
    expect_silent(sid <- H5Screate_simple(c(1,1)) )
    expect_silent( tid <- H5Tcopy("H5T_C_S1") )
    
    expect_silent( H5Tset_size(tid, 10L) )
    expect_silent( aid <- H5Acreate(did, "volume", tid, sid) )
    expect_silent( H5Awrite(aid, "liter") )
    expect_silent( H5Aclose(aid) )
    expect_silent( aid <- H5Acreate(did, "time", tid, sid) )
    expect_silent( H5Awrite(aid, "seconds") )
    expect_silent( H5Aclose(aid) )
    
    expect_silent( H5Sclose(sid) )
    expect_silent( H5Dclose(did) )
    expect_silent( H5Fclose(fid) )
})

test_that("Attribute existance", {
    
    expect_silent( fid <- H5Fopen(h5File) )
    expect_silent( did <- H5Dopen(fid, "A") )
    
    expect_true( H5Aexists(did, "volume") )
    expect_true( H5Aexists(did, "time") )
    expect_false( H5Aexists(did, "foo") )
    
    expect_silent( H5Dclose(did) )
    expect_silent( H5Fclose(fid) )

})

test_that("Opening attributes", {
    
    expect_silent( fid <- H5Fopen(h5File) )

    expect_silent( aid <- H5Aopen_by_name(fid, objname = "A", name = "volume") )
    expect_output(print(aid), regexp = "volume")
    expect_silent( H5Aclose(aid) )
    expect_silent( aid <- H5Aopen_by_name(fid, objname = "A", name = "time") )
    expect_output(print(aid), regexp = "time")
    expect_silent( H5Aclose(aid) )
    
    expect_silent( aid <- H5Aopen_by_idx(fid, n = 0, objname = "A") )
    expect_identical(H5Aget_name(aid), "time")
    expect_silent( H5Aclose(aid) )
    expect_silent( aid <- H5Aopen_by_idx(fid, n = 1, objname = "A") )
    expect_identical(H5Aget_name(aid), "volume")
    expect_silent( H5Aclose(aid) )

    expect_silent( H5Fclose(fid) )
    
    ## doesn't cope well when a missing attribute is named
    ## H5Aopen_by_name(fid, objname = "A", name = "foobaa")
    
})

test_that("Deleting attributes", {
    
    expect_silent( fid <- H5Fopen(h5File) )
    expect_silent( did <- H5Dopen(fid, "A") )
    
    ## deleting existing attribute
    expect_true( H5Aexists(did, "volume") )
    expect_equal( H5Adelete(did, "volume"), 0 )
    expect_false( H5Aexists(did, "volume") )
    
    ## trying to delete non-existant attribute
    expect_equal( H5Adelete(did, "foobaa"), -1 )
    
    expect_silent( H5Dclose(did) )
    expect_silent( H5Fclose(fid) )
    
})