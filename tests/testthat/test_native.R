context("native")

test_that("H5Dread/write supports native", {
})

test_that("H5Dset/get_extent supports native", {
})

test_that("H5Pset/get_chunk supports native", {
})

test_that("H5Sset/get_simple_extent supports native", {
})

test_that("H5Sselect_hyperslab supports native", {
})

test_that("H5Sselect_index supports native", {
})

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

## test_that("misc. fixes work", {
##     h5 <- tempfile(fileext = ".h5")
##     h5createFile(file = h5)
##     h5createGroup(file = h5, group = "test")

##     m <- matrix(1:12, 3, 4)
##     h5write(m, file = h5, name = "test/complex")
##     m <- matrix(as.complex(1:12), 3, 4)
##     expect_error(
##         h5write(m, file = h5, name = "test/complex")
##         "Writing 'complex' not supported"
##     )
## })
