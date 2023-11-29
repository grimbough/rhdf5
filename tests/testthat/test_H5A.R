library(rhdf5)

h5File <- tempfile(pattern = "ex_H5A_", fileext = ".h5")
h5createFile(h5File)
h5write(1:15, h5File,"A")

expect_null( h5errorHandling(type = "suppress") )  

test_that("writing attributes is silent", {
    expect_silent(fid <- H5Fopen(h5File) )
    expect_silent(did <- H5Dopen(fid, "A") )
    expect_silent(sid <- H5Screate_simple(1) )
    expect_silent(sid2 <- H5Screate_simple(10) )
    expect_silent( tid <- H5Tcopy("H5T_C_S1") )
    
    ## fixed length strings
    expect_silent( H5Tset_size(tid, 10L) )
    expect_silent( aid <- H5Acreate(did, "volume", tid, sid) )
    expect_silent( H5Awrite(aid, "liter") )
    expect_silent( H5Aclose(aid) )
    expect_silent( aid <- H5Acreate(did, "time", tid, sid) )
    expect_silent( H5Awrite(aid, "seconds") )
    expect_silent( H5Aclose(aid) )
    
    ## 64-bit integers
    expect_silent( aid <- H5Acreate(did, "int64", "H5T_NATIVE_INT64", sid) )
    expect_silent( H5Awrite(aid, 2^32) )
    expect_silent( H5Aclose(aid) )
    
    ## unsigned 32-bit integers
    expect_silent( aid <- H5Acreate(did, "uint32", "H5T_NATIVE_UINT32", sid2) )
    expect_silent( H5Awrite(aid, c(1:9, 2^31)) )
    expect_silent( H5Aclose(aid) )
    
    expect_silent( H5Sclose(sid) )
    expect_silent( H5Sclose(sid2) )
    expect_silent( H5Dclose(did) )
    expect_silent( H5Fclose(fid) )
})


test_that("attributes exist", {
    
    expect_silent( fid <- H5Fopen(h5File) )
    expect_silent( did <- H5Dopen(fid, "A") )
    
    expect_true( H5Aexists(did, "volume") )
    expect_true( H5Aexists(did, "time") )
    expect_false( H5Aexists(did, "foo") )
    
    expect_silent( H5Dclose(did) )
    expect_silent( H5Fclose(fid) )

})

test_that("attributes can be opened and closed", {
    
    expect_silent( fid <- H5Fopen(h5File) )

    expect_silent( aid <- H5Aopen_by_name(fid, objname = "A", name = "volume") )
    expect_output(print(aid), regexp = "volume")
    expect_silent( H5Aclose(aid) )
    expect_silent( aid <- H5Aopen_by_name(fid, objname = "A", name = "time") )
    expect_output(print(aid), regexp = "time")
    expect_silent( H5Aclose(aid) )
    
    expect_silent( aid <- H5Aopen_by_idx(fid, n = 2, objname = "A") )
    expect_identical(H5Aget_name(aid), "time")
    expect_silent( H5Aclose(aid) )
    expect_silent( aid <- H5Aopen_by_idx(fid, n = 4, objname = "A") )
    expect_identical(H5Aget_name(aid), "volume")
    expect_silent( H5Aclose(aid) )

    expect_silent( H5Fclose(fid) )
    
    ## doesn't cope well when a missing attribute is named
    ## H5Aopen_by_name(fid, objname = "A", name = "foobaa")
    
})

test_that("64-bit integer attributes are read correctly", {
    
    fid <- H5Fopen(h5File)
    did <- H5Dopen(fid, name = "A")
    aid <- H5Aopen(did, name = "int64")
    
    expect_warning(H5Aread(aid, bit64conversion = "int")) %>%
        is.na() %>%
        expect_true()
    
    expect_silent(H5Aread(aid, bit64conversion = "double")) %>%
        expect_equivalent(2^32)
    
    expect_silent(H5Aread(aid, bit64conversion = "bit64")) %>%
        expect_equivalent(bit64::as.integer64(2^32))
    
    H5Aclose(aid)
    H5Dclose(did)
    H5Fclose(fid)
    
})

test_that("unsigned 32-bit integer attributes are read correctly", {
    
    fid <- H5Fopen(h5File)
    did <- H5Dopen(fid, name = "A")
    aid <- H5Aopen(did, name = "uint32")
    
    expect_warning(H5Aread(aid, bit64conversion = "int")) |>
        is.na() |> any() |>
        expect_true()
    
    expect_silent(H5Aread(aid, bit64conversion = "double")) |>
        expect_equivalent(c(1:9, 2^31))
    
    expect_silent(x3 <- H5Aread(aid, bit64conversion = "bit64")) 
    expect_equivalent(x3, bit64::as.integer64(c(1:9, 2^31)))
    expect_is(x3, 'integer64')
    
    H5Aclose(aid)
    H5Dclose(did)
    H5Fclose(fid)
    
})

test_that("attributes can be deleted", {
    
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

test_that("fixed length string attributes are correct", {
  
  attr_value <- "Testing"
  attr_name <- "name"
  
  # create a string datatype with size of 7 bytes
  tid <- H5Tcopy("H5T_C_S1")
  H5Tset_strpad(tid, strpad = "NULLPAD")
  H5Tset_size(tid, nchar(attr_value)) 
  
  fid <- H5Fopen(h5File)
  sid <- H5Screate("H5S_SCALAR")
  aid <- H5Acreate(fid, name = attr_name, 
                   dtype_id=tid, h5space = sid)

  H5Awrite(aid, attr_value) # string of length 7
  
  H5Aclose(aid)
  H5Sclose(sid)
  H5Fclose(fid)
  
  attr <- h5readAttributes(h5File, "/")
  expect_is(attr, class = "list")
  expect_equal(names(attr), attr_name)
  expect_equal(attr$name, attr_value)
})
