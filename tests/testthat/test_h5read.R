library(rhdf5)

############################################################
context("h5read")
############################################################

set.seed(1234)
A = 1L:7L;  
B = matrix(1:18, ncol = 2); 
C = c(TRUE, TRUE, FALSE, NA)
D = seq(0, 1, by=0.1)
E = as.raw(sample(0:255, size = 5))
attr(D, "scale") <- "centimeters"
G = data.frame("col_A" = 1:10, "col_B" = letters[1:10], "col_C" = as.raw(1:10))

## output file name
h5File <- tempfile(pattern = "ex_read", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

# create file with group hierarchy  
h5createFile(h5File)
h5createGroup(file = h5File, group = "foo")
h5createDataset(file = h5File, dataset = "foo/A", dims = c(1, length(A) ), storage.mode = "integer")
h5write(obj = A, file = h5File, name = "foo/A")
h5createDataset(file = h5File, dataset = "foo/B", dims = c(2, length(B)/2 ))
h5write(obj = B, file = h5File, name = "foo/B")
h5createDataset(file = h5File, dataset = "baa", dims = c(1, length(D) ))
h5write(obj = D, file = h5File, name = "baa", write.attributes = TRUE)
h5write(obj = C, file = h5File, name = "logi")
h5write(obj = E, file = h5File, name = "raw")
h5write(obj = G, file = h5File, name = "data.frame")

test_that("Reading a dataset", {
    
    expect_silent(baa <- h5read(h5File, name = "baa", read.attributes = TRUE)) %>%
    expect_is("matrix" )
    expect_identical( dim(baa), c(1L, length(D)) )
    
    expect_silent(C2 <- h5read(h5File, name = "logi")) %>%
        expect_is("array") %>%
        expect_identical(as.array(C))
    expect_identical(storage.mode(C2), "logical")
    
    expect_silent(E2 <- h5read(h5File, name = "raw")) %>%
      expect_is("array") %>%
      expect_identical(as.array(E2))
    expect_identical(storage.mode(E2), "raw")
})

test_that("Reading a group", {
    
    foo <- h5read(h5File, name = "foo")
    expect_is( foo, "list" )
    expect_equal( length(foo), 2 )
    expect_true( all(c("A", "B") %in% names(foo)) )
})

test_that("Reading a nested dataset", {
    
    fooA <- h5read(h5File, name = "foo/A")
    expect_is( fooA, "matrix" )
    expect_equal( dim(fooA), c(1, length(A)) )
})

test_that("Dropping dimensions", {
    
    fooA <- h5read(h5File, name = "foo/A", drop = TRUE)
    expect_is( fooA, "integer" )
    expect_null( dim(fooA) )
    expect_equal( fooA, A )
    
    ## this drops for matrices too
    fooB <- h5read(h5File, name = "foo/B", drop = TRUE)
    expect_is( fooB, "numeric" )
    expect_null( dim(fooB) )
    expect_equal( fooB, as.numeric(B) )
})

test_that("Reading attributes too", {
    
    baa <- h5read(h5File, name = "baa", read.attributes = TRUE)
    expect_equal( as.character(attributes(baa)$scale), attributes(D)$scale )
})

test_that("Error if file doesn't exist", {
    
    expect_error( h5read(file = "/foo/baa.h5", name = "missing"),
                  regexp = "does not exist.$")
    
})

test_that("Error if asking for something that isn't there", {
    
    expect_error( h5read(file = h5File, name = "missing"),
                  regexp = "does not exist in this HDF5 file.$")
    
})



test_that("writing & reading empty vectors", {
    
    h5File <- tempfile(pattern = "ex_read", fileext = ".h5")
    expect_true(h5createFile(h5File))
    expect_silent(h5write(obj = character(0), file = h5File, name = "char"))
    expect_silent(h5write(obj = integer(0), file = h5File, name = "int"))
    expect_silent(h5write(obj = double(0), file = h5File, name = "double"))
    expect_silent(h5write(obj = logical(0), file = h5File, name = "logical"))
    expect_silent(h5write(obj = raw(0), file = h5File, name = "raw"))
    #expect_silent(h5write(obj = factor(levels = c("L1", "L2")), file = h5File, name = "factor"))
    
    expect_silent(tmp <- h5read(file = h5File, name = "char")) %>%
        expect_is("character") %>%
        expect_length(0)
    expect_silent(h5read(file = h5File, name = "int")) %>%
        expect_is("array") %>%
        expect_length(0) %>%
        storage.mode() %>% expect_identical("integer")  
    expect_silent(h5read(file = h5File, name = "double")) %>%
        expect_is("array") %>%
        expect_length(0) %>%
        storage.mode() %>% expect_identical("double")  
    expect_silent(h5read(file = h5File, name = "logical")) %>%
        expect_is("array") %>%
        expect_length(0) %>%
        storage.mode() %>% expect_identical("logical")  
    expect_silent(h5read(file = h5File, name = "raw")) %>%
        expect_is("array") %>%
        expect_length(0) %>%
        storage.mode() %>% expect_identical("raw") 

})

test_that("writing & reading empty arrays", {
    
    h5File <- tempfile(pattern = "ex_read", fileext = ".h5")
    expect_true(h5createFile(h5File))
    expect_silent(h5write(obj = matrix(nrow = 0, ncol = 0), file = h5File, name = "mat"))

    expect_silent(tmp <- h5read(file = h5File, name = "mat")) %>%
        expect_is("matrix") %>%
        expect_length(0)
})

test_that("reading & writing scalar dataspaces", {
    h5File <- tempfile(pattern = "ex_read", fileext = ".h5")
    expect_silent(fid <- H5Fcreate(h5File))
    expect_silent(sid <- H5Screate(type = "H5S_SCALAR"))
    expect_silent(tid <- H5Tcopy("H5T_C_S1"))
    expect_silent(H5Tset_size(tid, 12))
    expect_silent(did <- H5Dcreate(fid, "scalar", tid, h5space = sid))
    expect_silent(H5Dwrite(did, "test string"))
    expect_silent(H5Dclose(did))
    expect_silent(H5Sclose(sid))
    expect_silent(H5Fclose(fid))
    
})

############################################################
context("NA values")
############################################################

## output file name
h5File <- tempfile(pattern = "ex_read", fileext = ".h5")
if(file.exists(h5File))
  file.remove(h5File)

h5createFile(h5File)

test_that("NA characters are supported", {
  
  expect_silent(h5write(c(NA_character_, LETTERS), h5File, "NA_char"))
  expect_true( any(is.na(h5read(h5File, name = "NA_char")) ) )
  
  expect_warning(h5write(c(NA_character_, LETTERS, "NA"), h5File, "NA_char_2"),
                 "Both NA_character_ and the string 'NA' detected")
})

test_that("No warnings are generated for integers containing NA written from R", {
  
  expect_silent(h5write(c(NA_integer_, 1:5), h5File, "NA_int"))
  expect_silent( na_int <- h5read(h5File, name = "NA_int") )
  expect_true( is.na( na_int[1] ) )
  
})
  
test_that("Warnings are generated for integers containing NA written outside rhdf5", {
  ## remove the rhdf5-NA.OK attribute to simulate data not written by rhdf5
  fid <- H5Fopen(name = h5File)
  did <- H5Dopen(fid, name = "NA_int")
  H5Adelete(did, "rhdf5-NA.OK")
  H5Dclose(did)
  H5Fclose(fid)
  expect_message(h5read(h5File, name = "NA_int"),
                 "The value -2^31 was detected in the dataset", 
                 fixed = TRUE)
})

############################################################
context("indexing")
############################################################

A = matrix(1:100, ncol = 10); 
B = array(1:1000, dim = c(10,10,10))

## output file name
h5File <- tempfile(pattern = "ex_read", fileext = ".h5")

# create file with group hierarchy  
h5createFile(h5File)
h5write(obj = A, file = h5File, name = "A")
h5write(obj = B, file = h5File, name = "B")

test_that("Works with a dimension of length 1", {
    expect_silent(A2 <- h5read(h5File, name = "A", index = list(NULL, 5)))
    expect_is(A2, "matrix")
    expect_equal(ncol(A2), 1L)
    expect_equal(nrow(A2), 10L)
    expect_identical(A2[,1], A[,5])
})


test_that("indices that resolve to a single hyperslab are ok", {
  expect_silent(A2 <- h5read(h5File, name = "A", index = list(2:3, c(1,2,4,5))))
  expect_is(A2, "matrix")
  expect_equal(ncol(A2), 4)
  expect_equal(nrow(A2), 2)
  expect_identical(A2, A[2:3,c(1,2,4,5)])
})

test_that("Columns specified multiple times", {
  expect_silent(A2 <- h5read(h5File, name = "A", index = list(NULL, c(9,1,1,5))))
  expect_equal(ncol(A2), 4L)
  expect_equal(nrow(A2), 10L)
  expect_identical(A2[,2], A[,1])
})


test_that("Indexing multiple dimensions works", {
    expect_silent(A2 <- h5read(h5File, name = "A", index = list(c(1,3), c(1,5,10))))
    expect_equal(ncol(A2), 3L)
    expect_equal(nrow(A2), 2L)
    expect_equal(A2[2,3], A[3,10])
    
    expect_silent(B2 <- h5read(h5File, name = "B", index = list(c(1,8,10), NULL, NULL)))
    expect_equal(dim(B2), c(3,10,10))
    expect_identical(B2[3,,], B[10,,])
    expect_silent(B2 <- h5read(h5File, name = "B", index = list(c(8), NULL, c(4,6))))
    expect_equal(dim(B2), c(1,10,2))
    expect_identical(B2[1,6,], B[8,6,c(4,6)])
})


test_that("Empty index retain dimensionality", {
    expect_silent(A2 <- h5read(h5File, name = "A", index = list(integer(0), 5))) %>%
        expect_is("matrix")
    expect_equal(dim(A2), c(0,1))
    expect_silent(A2 <- h5read(h5File, name = "A", index = list(integer(0), 1:5)))  %>%
        expect_is("matrix")
    expect_equal(dim(A2), c(0,5))
    expect_silent(A2 <- h5read(h5File, name = "A", index = list(integer(0), integer(0))))  %>%
        expect_is("matrix")
    expect_equal(dim(A2), c(0,0))
    
    expect_silent(B2 <- h5read(h5File, name = "B", index = list(integer(0), integer(0), 5))) %>%
        expect_is("array")
    expect_equal(dim(B2), c(0,0,1))
    expect_silent(B2 <- h5read(h5File, name = "B", index = list(integer(0), 1:5, 6)))  %>%
        expect_is("array")
    expect_equal(dim(B2), c(0,5,1))
    expect_silent(B2 <- h5read(h5File, name = "B", index = list(integer(0), integer(0), integer(0))))  %>%
        expect_is("array")
    expect_equal(dim(B2), c(0,0,0))
})

############################################################
context("64-bit conversion")
############################################################

## output file name
h5File <- tempfile(pattern = "ex_read", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

# create file with integers of different types
h5createFile(h5File)
h5createDataset(h5File, "int32", dims=50, storage.mode="integer")
h5createDataset(h5File, "int64", dims=51, storage.mode="integer64")
h5createDataset(h5File, "uint32", dims=50, H5type = "H5T_STD_U32LE")
h5write(obj = 1:50, file = h5File, name = "int32")
h5write(obj = c(1:50, 2^32), file = h5File, name = "int64")
h5write(obj = 2^31 + 1:50, file = h5File, name = "uint32")

test_that("Signed 32bit integers are unchanged for all conversion arguments", {
    
    expect_is(x1 <- h5read(h5File, name = "int32", bit64conversion = "int"), "array")
    expect_equal(storage.mode(x1), "integer")
    expect_is(x2 <- h5read(h5File, name = "int32", bit64conversion = "double"), "array")
    expect_equal(storage.mode(x2), "integer")
    expect_is(x3 <- h5read(h5File, name = "int32", bit64conversion = "bit64"), "array")
    expect_equal(storage.mode(x3), "integer")
    
    expect_identical(x1, x2)
    expect_identical(x1, x3)
})

test_that("signed 64-bit integers are converted", {
    
    expect_is(x1 <- h5read(h5File, name = "int64", bit64conversion = "int"), "array") %>%
      expect_warning()
    expect_identical(x1[51], NA_integer_)
    expect_equal(storage.mode(x1), "integer")
    
    expect_is(x2 <- h5read(h5File, name = "int64", bit64conversion = "double"), "array")
    expect_identical(x2, array(c(1:50, 2^32)))
    expect_equal(storage.mode(x2), "double")
    
    expect_is(x3 <- h5read(h5File, name = "int64", bit64conversion = "bit64"), "integer64")
    expect_identical(x3, as.array(bit64::as.integer64(c(1:50, 2^32))))
    expect_equal(storage.mode(x3), "double")
    
})

test_that("Unsigned 32bit integers are converted to NA out of range", {
    
    expect_warning(x1 <- h5read(h5File, name = "uint32", bit64conversion = "int")) 
    expect_true(all(is.na(x1)))
})

test_that("Unsigned 32bit integers are converted properly to double/bit64", {
    
    expect_is(x2 <- h5read(h5File, name = "uint32", bit64conversion = "double"), "array") 
    expect_equal(storage.mode(x2), "double")
    expect_equivalent(x2, 2^31 + 1:50)
    
    expect_is(x3 <- h5read(h5File, name = "uint32", bit64conversion = "bit64"), "integer64")
    expect_equal(storage.mode(x3), "double")
    expect_equal(class(x3), "integer64")
    expect_true(all(x3 > 2^31))
})

############################################################
context("Using filters other than ZLIB")
###########################################################

test_that("Reading SZIP", {
  
    szip_file <- system.file("testfiles", "h5ex_d_szip.h5", package = "rhdf5")
    expect_silent(h5read(szip_file, "DS1")) %>%
        expect_is("matrix") %>% 
        dim() %>% expect_equal(c(64,32))
})

test_that("Failing to read BLOSC", {
    blosc_file <- system.file("testfiles", "h5ex_d_blosc.h5", package = "rhdf5")
    expect_silent(h5read(blosc_file, "dset"))
})

############################################################

test_that("No open HDF5 objects are left", {
    expect_equal( length(h5validObjects()), 0 )
})

