library(rhdf5)

############################################################
context("H5: basic HDF5 functions")
############################################################

test_that("General library functions", {

    expect_silent( H5open() )
    expect_silent( H5garbage_collect() )
    expect_silent( H5close() )
    expect_is( H5get_libversion(), "integer" ) %>%
        expect_named( c("majnum", "minnum", "relnum") )
})
