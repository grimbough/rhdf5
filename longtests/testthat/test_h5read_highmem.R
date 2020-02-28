library(rhdf5)

############################################################
context("h5read_high_memory")
############################################################

## output file name
h5File <- tempfile(pattern = "ex_read", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

# create file
h5createFile(h5File)

test_that("Read / write vector longer than 2^31-1", {

    ## travis doesn't have resources to create an 8GB vector, so we skip there
    long_vector <- tryCatch(integer(length = (2^31)+1000),
                            error = function(e) NULL)

    if(!is.null(long_vector)) {
        expect_silent(
            h5createDataset(file = h5File, dataset = "too_long", dims = length(long_vector),
                            level = 0, storage.mode = "integer", chunk = 1e6)
        )
        expect_silent(
            h5write(obj = long_vector, file = h5File, name = "too_long")
        )

        rm(long_vector)

        expect_silent( tmp <- h5read(file = h5File, name = "too_long") ) %>%
            expect_is("integer") %>%
            length() %>%
            expect_equal((2^31)+1000)
    }
})



