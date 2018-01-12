context("native")

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

test_that("h5read native non-R hdff5 files", {
    enum <- system.file("testfiles", "h5ex_t_enum.h5", package="rhdf5")
    arr <- system.file("testfiles", "h5ex_t_array.h5", package="rhdf5")
    compound <- system.file("testfiles", "h5ex_t_cmpd.h5", package="rhdf5")

    do_reads <- function(file) {
        m1 <- h5read(file = file, name = "/DS1", native = TRUE)
        m2 <- h5read(file = file, name = "/DS1", native = FALSE)
        expect_equivalent(m1, t(m2))
    }

    do_reads(enum)
    #do_reads(arr)
    #do_reads(compound)
})

test_that("H5F native functionality", {
    h5 <- tempfile(fileext = ".h5")
    h5createFile(file = h5)
    h5createGroup(file = h5, group = "test")

    A <- matrix(1:10, nr=5, nc=2)
    h5write(A, h5, "test/A", naitve=TRUE)

    h5f <- H5Fopen(h5, native=TRUE)

    h5d <- h5f&"/test/A"
    m0 <- h5d[,]

    expect_equivalent(m0, A)
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
