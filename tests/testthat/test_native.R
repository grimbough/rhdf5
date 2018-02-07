library(rhdf5)

############################################################
context("h5create supports native funcitonality")
############################################################

test_that("H5create supports native", {
    h5file <- tempfile(pattern = "ex_save", fileext = ".h5")
    if(file.exists(h5file))
        file.remove(h5file)

    expect_true( h5createFile(file = h5File, native = TRUE) )
    expect_true( file.exists(h5File) )
    

    expect_true( h5createDataset(file = h5File, dataset = "A", dims = c(2,1)), native = TRUE )
    expect_true( "A" %in% names(h5dump(file = h5File)) )
    A <- h5read(file = h5File, name = "A", native = TRUE)
    
    expect_is(A, "matrix")
    expect_true(nrow(A) == 2)
    expect_true(ncol(A) == 1)
    expect_is(A[1,1], "numeric")
})

############################################################
context("H5ls supports native functionality")
############################################################

test_that("H5ls supports native", {
    h5file <- tempfile(fileext = ".h5")
    h5createFile(file = h5file)
    h5createGroup(file = h5file, group = "test")
    m0 <- matrix(1:12, 3, 4)

    h5write(m0, file = h5file, name = "test/native", native=TRUE)
    h5write(m0, file = h5file, name = "test/nonnative", native=FALSE)

    xx <- h5ls(h5file)
    object <- subset(xx, group == "/test" & name == "nonnative", "dim")[[1]]
    expect_equal(object, "3 x 4")
    object <- subset(xx, group == "/test" & name == "native", "dim")[[1]]
    expect_equal(object, "4 x 3")

    xx <- h5ls(h5file, native = TRUE)
    object <- subset(xx, group == "/test" & name == "nonnative", "dim")[[1]]
    expect_equal(object, "4 x 3")
    object <- subset(xx, group == "/test" & name == "native", "dim")[[1]]
    expect_equal(object, "3 x 4")

    m0 <- array(1:24, c(2, 3, 4))
    h5write(m0, file = h5file, name = "test/native-array", native=TRUE)
    h5write(m0, file = h5file, name = "test/nonnative-array", native=FALSE)

    xx <- h5ls(h5file)
    object <-
        subset(xx, group == "/test" & name == "nonnative-array", "dim")[[1]]
    expect_equal(object, "2 x 3 x 4")
    object <-
        subset(xx, group == "/test" & name == "native-array", "dim")[[1]]
    expect_equal(object, "4 x 3 x 2")

    xx <- h5ls(h5file, native = TRUE)
    object <-
        subset(xx, group == "/test" & name == "nonnative-array", "dim")[[1]]
    expect_equal(object, "4 x 3 x 2")
    object <-
        subset(xx, group == "/test" & name == "native-array", "dim")[[1]]
    expect_equal(object, "2 x 3 x 4")
})

test_that("h5read/writeDataset supports native", {
})

############################################################
context("h5read/h5write supports native functionality")
############################################################

test_that("h5read/write supports native", {
    h5 <- tempfile(fileext = ".h5")
    h5createFile(file = h5)
    h5createGroup(file = h5, group = "test")

    ## matrix
    do_matrix <- function(values, nrow, ncol, val) {
        m0 <- matrix(values, nrow, ncol)

        h5write(m0, file = h5, name = paste0("test/nonnative", val),
                native=FALSE)
        m1 <- h5read(file = h5, name = paste0("test/nonnative", val),
                     native = TRUE)
        expect_equivalent(m0, t(m1))
        m2 <- h5read(file = h5, name = paste0("test/nonnative", val),
                     native = FALSE)
        expect_equivalent(m0, m2)

        h5write(m0, file = h5, name = paste0("test/native", val), native=TRUE)
        m1 <- h5read(file = h5, name = paste0("test/native", val),
                     native = FALSE)
        expect_equivalent(m0, t(m1))
        m2 <- h5read(file = h5, name = paste0("test/native", val),
                     native = TRUE)
        expect_equivalent(m0, m2)
    }
    do_matrix(1:12, 3, 4, "A")
    do_matrix(as.numeric(1:12), 3, 4, "B")
    do_matrix(sample(c(TRUE, FALSE), 12, replace=TRUE), 3, 4, "C")
    do_matrix(LETTERS[1:12], 3, 4, "D")

    ## 3D-array
    do_array <- function(values, dim, val) {
        m0 <- array(values, dim)

        h5write(m0, file = h5, name = paste0("test/nonnative-array", val),
                native=FALSE)
        m1 <- h5read(file = h5, name = paste0("test/nonnative-array", val),
                     native = TRUE)
        expect_equivalent(m0, aperm(m1))
        m2 <- h5read(file = h5, name = paste0("test/nonnative-array", val),
                     native = FALSE)
        expect_equivalent(m0, m2)

        h5write(m0, file = h5, name = paste0("test/native-array", val),
                native=TRUE)
        m1 <- h5read(file = h5, name = paste0("test/native-array", val),
                     native = FALSE)
        expect_equivalent(m0, aperm(m1))
        m2 <- h5read(file = h5, name = paste0("test/native-array", val),
                     native = TRUE)
        expect_equivalent(m0, m2)
    }

    do_array(1:12, c(2, 3, 4), "A")
    do_array(as.numeric(1:12), c(2, 3, 4), "B")
    do_array(sample(c(TRUE, FALSE), 12, replace=TRUE), c(2, 3, 4), "C")
    do_array(LETTERS[1:12], c(2, 3, 4), "D")
})

############################################################
context("h5read supports native functionality misc.")
############################################################

test_that("h5read native non-R hdff5 files", {
    enum <- system.file("testfiles", "h5ex_t_enum.h5", package="rhdf5")
    arr <- system.file("testfiles", "h5ex_t_array.h5", package="rhdf5")
    compound <- system.file("testfiles", "h5ex_t_cmpd.h5", package="rhdf5")

    # Test enums type
    m1 <- h5read(file = enum, name = "/DS1", native = TRUE)
    m2 <- h5read(file = enum, name = "/DS1", native = FALSE)
    expect_equivalent(m1, t(m2))

    # Test arrays type
    m1 <- h5read(file = arr, name = "/DS1", native = TRUE)
    m2 <- h5read(file = arr, name = "/DS1", native = FALSE)
    expect_equivalent(m1, aperm(m2))

    # Test compound type (currently native provides no different functionality)
    m1 <- h5read(file = compound, name = "/DS1", native = TRUE)
    m2 <- h5read(file = compound, name = "/DS1", native = FALSE)
    expect_equivalent(m1, m2)
})

############################################################
context("H5Screate supports native Functionality")
############################################################

test_that("H5Screate supports native", {    
    expect_silent(sid <- H5Screate(native = TRUE))
    H5Sset_extent_simple(sid, c(10, 20, 30))
    size <- H5Sget_simple_extent_dims(sid)['size']
    expectIdentical(size, c(10, 20, 30))

    expect_silent(sid <- H5Screate(native = FALSE))
    H5Sset_extent_simple(sid, c(10, 20, 30))
    size <- H5Sget_simple_extent_dims(sid)['size']
    expectIdentical(size, c(30, 20, 10))
    
    expect_silent(sid <- H5Screate_simple(c(10, 20, 30), native = TRUE))
    size <- H5Sget_simple_extent_dims(sid)['size']
    expect_identical(size, c(10, 20, 30))

    expect_silent(sid <- H5Screate_simple(c(10, 20, 30), native = FALSE))
    size <- H5Sget_simple_extent_dims(sid)['size']
    expect_identical(size, c(30, 20, 10))
})

############################################################
context("H5Sselect_hyperslab supports native functionality")
############################################################

test_that("Selecting hyperslabs", {
    
    expect_silent(sid <- H5Screate_simple(dims = c(10,20), native = TRUE))

    expect_silent( H5Sselect_hyperslab(sid) )
    
    expect_silent(H5Sclose(sid))
})

############################################################
context("H5Sselect_index supports native functionality")
############################################################

test_that("Selecting using an index", {
    
    expect_silent(sid <- H5Screate_simple(dims = c(10,20,30)))
    
    expect_silent( size <- H5Sselect_index(sid, index = list(1:5, 1:5, 11:15)) )
    expect_identical( size, c(5,5,5) )
    
    expect_silent( size <- H5Sselect_index(sid, index = list(NULL, NULL, NULL)) )
    expect_identical( size, c(10,20,30) )
    
    ## errors when not providing enough dimensions or incorrect dimensions
    expect_error( H5Sselect_index(sid, index = list(10)),
                  regexp = "length of list index not equal to h5space dimensional extension")
    expect_error( H5Sselect_index(sid, index = list(1:5, 1:5, 0:5)), 
                  regexp = "negative indices and 0 not supported" )
    expect_error( H5Sselect_index(sid, index = list(1:15, 1:5, 1:5)), 
                  regexp = "index exceeds HDF5-array dimension")
    
    expect_silent(H5Sclose(sid))
})

############################################################
context("H5F supports native functionality")
############################################################

test_that("H5F native functionality", {
    for (native in c(FALSE, TRUE)) {
        h5 <- tempfile(fileext = ".h5")
        h5createFile(file = h5)
        h5createGroup(file = h5, group = "test")

        A <- matrix(1:10, nr=5, nc=2)
        h5write(A, h5, "test/A", native=native)

        h5f <- H5Fopen(h5, native=native)
        h5d <- h5f&"/test/A"

        m0 <- h5d[,]
        expect_equivalent(m0, A)
        expect_equivalent(m0[1,], A[1,])

        h5d[,1] <- 11:15
        expect_equivalent(h5d[,1], 11:15)

        H5Oclose(h5d)
        H5Fclose(h5f)
    }
})

