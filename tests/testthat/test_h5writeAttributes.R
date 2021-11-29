library(rhdf5)

############################################################
context("h5writeAttributes")
############################################################

## output file name
h5File <- tempfile(pattern = "ex_attr_", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

h5createFile(file = h5File)

test_that("Adding attribute to file", {
    fid <- H5Fopen(h5File)
    
    ## character
    attr <- "foo"
    expect_silent(h5writeAttribute(attr = attr, h5obj = fid, name = "char_attr"))
    ## integer
    attr <- 1L
    expect_silent(h5writeAttribute(attr = attr, h5obj = fid, name = "int_attr"))
    ## numeric
    attr <- 10.0
    expect_silent(h5writeAttribute(attr = attr, h5obj = fid, name = "numeric_attr"))
    ## matrix
    attr <- matrix(1:10, nrow = 2)
    expect_silent(h5writeAttribute(attr = attr, h5obj = fid, name = "matrix_attr"))
    
    H5Fclose(fid)
    
    attr_back <- h5readAttributes(h5File, name = "/")
    expect_length(attr_back, n = 4)
    expect_true( all(c("char_attr", "int_attr", "numeric_attr", "matrix_attr") %in% names(attr_back)) )
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
    expect_silent(h5writeAttribute(attr = attr, h5obj = gid, name = "char_attr"))
    ## integer
    attr <- 1L
    expect_silent(h5writeAttribute(attr = attr, h5obj = gid, name = "int_attr"))
    ## numeric
    attr <- 10.0
    expect_silent(h5writeAttribute(attr = attr, h5obj = gid, name = "numeric_attr"))
    ## matrix
    attr <- matrix(1:10, nrow = 2)
    expect_silent(h5writeAttribute(attr = attr, h5obj = gid, name = "matrix_attr"))
    
    H5Gclose(gid)
    H5Fclose(fid)
    
    attr_back <- h5readAttributes(h5File, name = "foo_group")
    expect_length(attr_back, n = 4)
    expect_true( all(c("char_attr", "int_attr", "numeric_attr", "matrix_attr") %in% names(attr_back)) )
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

test_that("Checking other string options when adding attributes", {
    h5createGroup(file = h5File, group = "blah_group")
    fid <- H5Fopen(h5File)
    gid <- H5Gopen(h5loc = fid, name = "blah_group")

    ## variable strings
    attr <- "blah"
    h5writeAttribute(attr = attr, h5obj = gid, name = "char_attr", variableLengthString = TRUE)
    
    ## different encoding
    attr <- "blah2"
    h5writeAttribute(attr = attr, h5obj = gid, name = "char_attr2", cset = "UTF-8")

    ## as a scalar
    attr <- "blah3"
    h5writeAttribute(attr = attr, h5obj = gid, name = "char_attr3", cset = "UTF-8", asScalar = TRUE)
    expect_error(h5writeAttribute(attr = c(attr, attr), h5obj = gid, name = "char_attr3", cset = "UTF-8", asScalar = TRUE), "cannot use")

    H5Gclose(gid)
    H5Fclose(fid)

    attr_back <- h5readAttributes(h5File, name = "blah_group")
    expect_length(attr_back, n = 3)

    expected <- c("char_attr", "char_attr2", "char_attr3")
    expect_identical(sort(expected), sort(names(attr_back)))
    expect_identical(unname(unlist(attr_back[expected])), c("blah", "blah2", "blah3"))
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
