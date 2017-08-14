library(rhdf5)

############################################################
context("h5writeAttributes")
############################################################

## output file name
h5File <- tempfile(pattern = "ex_attr_", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

h5createFile(file = h5File)
A = array(seq(0.1,2.0,by=0.1),dim=c(5,2,2))
h5write(A, h5File,"A")

test_that("Adding attribute to file", {
    fid <- H5Fopen(h5File)
    
    ## character
    attr <- "foo"
    h5writeAttribute(attr = attr, h5obj = fid, name = "char_attr")
    ## integer
    attr <- 1L
    h5writeAttribute(attr = attr, h5obj = fid, name = "int_attr")
    ## numeric
    attr <- 10.0
    h5writeAttribute(attr = attr, h5obj = fid, name = "numeric_attr")
    ## matrix
    attr <- matrix(1:10, nrow = 2)
    h5writeAttribute(attr = attr, h5obj = fid, name = "matrix_attr")
    
    H5Fclose(fid)
    
    attr_back <- h5readAttributes(h5File, name = "/")
    expect_length(attr_back, n = 4)
    expect_true( all(names(attr_back) %in% c("char_attr", "int_attr", "numeric_attr", "matrix_attr")) )
    expect_is(attr_back$char_attr[1], "character")
    expect_is(attr_back$int_attr[1], "integer")
    expect_is(attr_back$numeric_attr[1], "numeric")
})

test_that("Adding attribute to group", {
    
    h5createGroup(file = h5File, group = "foo_group")
    fid <- H5Fopen(h5File)
    gid <- H5Gopen(h5loc = fid, name = "foo_group")
    
    ## character
    attr <- "foo"
    h5writeAttribute(attr = attr, h5obj = gid, name = "char_attr")
    ## integer
    attr <- 1L
    h5writeAttribute(attr = attr, h5obj = gid, name = "int_attr")
    ## numeric
    attr <- 10.0
    h5writeAttribute(attr = attr, h5obj = gid, name = "numeric_attr")
    ## matrix
    attr <- matrix(1:10, nrow = 2)
    h5writeAttribute(attr = attr, h5obj = gid, name = "matrix_attr")
    
    H5Gclose(gid)
    H5Fclose(fid)
    
    attr_back <- h5readAttributes(h5File, name = "foo_group")
    expect_length(attr_back, n = 4)
    expect_true( all(names(attr_back) %in% c("char_attr", "int_attr", "numeric_attr", "matrix_attr")) )
    expect_is(attr_back$char_attr[1], "character")
    expect_is(attr_back$int_attr[1], "integer")
    expect_is(attr_back$numeric_attr[1], "numeric")
})

test_that("Adding attribute to dataset", {
    
    h5createDataset(file = h5File, dataset = "baa_dataset", dims = c(2,2))
    fid <- H5Fopen(h5File)
    did <- H5Dopen(fid, name = "baa_dataset")
    
    ## character
    attr <- "foo"
    h5writeAttribute(attr = attr, h5obj = did, name = "char_attr")
    ## integer
    attr <- 1L
    h5writeAttribute(attr = attr, h5obj = did, name = "int_attr")
    ## numeric
    attr <- 10.0
    h5writeAttribute(attr = attr, h5obj = did, name = "numeric_attr")
    ## matrix
    attr <- matrix(1:10, nrow = 2)
    h5writeAttribute(attr = attr, h5obj = did, name = "matrix_attr")
    
    H5Dclose(did)
    H5Fclose(fid)
    
    attr_back <- h5readAttributes(h5File, name = "baa_dataset")
    expect_length(attr_back, n = 4)
    expect_true( all(names(attr_back) %in% c("char_attr", "int_attr", "numeric_attr", "matrix_attr")) )
    expect_is(attr_back$char_attr[1], "character")
    expect_is(attr_back$int_attr[1], "integer")
    expect_is(attr_back$numeric_attr[1], "numeric")
})

test_that("Unable to add logical attribute", {
    fid <- H5Fopen(h5File)
    expect_error( h5writeAttribute(attr = FALSE, h5obj = fid, name = "logical_attr"))  
    H5Fclose(fid)
})

test_that("Overwrite exisiting attribute", {
    fid <- H5Fopen(h5File)
    expect_silent( h5writeAttribute(attr = "new_character", h5obj = fid, name = "char_attr") )
    H5Fclose(fid)
    
    attr_list <- h5readAttributes(h5File, name = "/")
    expect_identical( attr_list$char_attr[1], "new_character" )
})

test_that("No open HDF5 objects are left", {
    expect_equal( length(h5validObjects()), 0 )
})