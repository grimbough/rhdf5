library(rhdf5)

############################################################
context("h5 defined constants")
############################################################

test_that("const groups", {
    ## No longer exported---must stop testing
    ## expect_is( H5loadConstants(), "list" ) %>%
    ##     expect_length( n = 18 )
    
    expect_is( h5constType(), "character" ) %>%
        expect_length(n = 19)
})

test_that("constants", {
    expect_is( h5const("H5F_ACC"), "character" ) %>%
        expect_length(n = 2)
    
    expect_null( h5const("foobaa") )
})

test_that("defaults", {
    expect_is( h5default("H5F_ACC"), "character" ) %>%
        expect_length(n = 1) %>%
        expect_identical(expected = "H5F_ACC_TRUNC")
    
    expect_null( h5default("foobaa") )
})

