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


## we might deprecate const2Factor and remove this later
# test_that("const2String similar to const2Factor", {
#   
#   const_types <- rhdf5::h5constType()
#   
#   for(const in const_types) {
#     const_values <- h5constants[[ const ]]
#     expect_equal(
#       as.character(rhdf5:::h5const2Factor(group = const, values = const_values)),
#       rhdf5:::h5const2String(group = const, values = const_values)
#     )
#   }
#   
# })
