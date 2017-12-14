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

    h5write(m0, file = h5file, name = "test/native", native=TRUE)
    h5write(m0, file = h5file, name = "test/nonnative", native=FALSE)

    xx <- h5ls(h5file)
    object <- subset(xx, group == "/test" & name == "nonnative", "dim")[[1]]
    expect_equal(object, "3 x 4")
    object <- subset(xx, group == "/test" & name == "native", "dim")[[1]]
    expect_equal(object, "4 x 3")

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
})

test_that("h5read/writeDataset supports native", {
})

test_that("h5read/write supports native", {
    h5 <- tempfile(fileext = ".h5")
    h5createFile(file = h5)
    h5createGroup(file = h5, group = "test")

    ## matrix
    do_matrix <- function(values, nrow, ncol) {
        m0 <- matrix(values, nrow, ncol)

        h5write(m0, file = h5, name = "test/nonnative", native=FALSE)
        m1 <- h5read(file = h5, name = "test/nonnative", native = TRUE)
        expect_identical(m0, t(m1))
        m2 <- h5read(file = h5, name = "test/nonnative", native = FALSE)
        expect_identical(m0, m2)

        h5write(m0, file = h5, name = "test/native", native=TRUE)
        m1 <- h5read(file = h5, name = "test/native", native = FALSE)
        expect_identical(m0, t(m1))
        m2 <- h5read(file = h5, name = "test/native", native = TRUE)
        expect_identical(m0, m2)
    }
    do_matrix(1:12, 3, 4)
    do_matrix(as.numeric(1:12), 3, 4)
    do_matrix(sample(c(TRUE, FALSE), 12), 3, 4)
    do_matrix(LETTERS[1:12], 3, 4)

    ## 3D-array
    do_array <- function(values, dim) {
        m0 <- array(values, dim)

        h5write(m0, file = h5, name = "test/nonnative-array", native=FALSE)
        m1 <- h5read(file = h5, name = "test/nonnative-array", native = TRUE)
        expect_identical(m0, t(m1))
        m2 <- h5read(file = h5, name = "test/nonnative-array", native = FALSE)
        expect_identical(m0, m2)

        h5write(m0, file = h5, name = "test/native-array", native=TRUE)
        m1 <- h5read(file = h5, name = "test/native-array", native = FALSE)
        expect_identical(m0, t(m1))
        m2 <- h5read(file = h5, name = "test/native-array", native = TRUE)
        expect_identical(m0, m2)
    }
    
    do_array(1:12, c(2, 3, 4))
    do_array(as.numeric(1:12), c(2, 3, 4))
    do_array(sample(c(TRUE, FALSE), 12), c(2, 3, 4))
    do_array(LETTERS[1:12], c(2, 3, 4))
})
