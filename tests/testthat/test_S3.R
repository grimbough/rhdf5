library(rhdf5)

private_S3_url <- "https://rhdf5-public.s3.eu-central-1.amazonaws.com/h5ex_t_array.h5"

test_that("h5ls() works for files in public S3 buckets", {
    expect_silent(h5ls_out <- h5ls(private_S3_url, s3 = TRUE))
    expect_is(h5ls_out, "data.frame")
    expect_true( "DS1" %in% h5ls_out$name )
})

test_that("h5dump() works for files in public S3 buckets", {
    expect_silent(h5dump_out <- h5dump(private_S3_url, s3 = TRUE))
    expect_is( h5dump_out, "list" )
    expect_equivalent( length(h5dump_out), 1 )
    expect_equivalent( dim(h5dump_out$DS1), c(5,3,4) )
})


test_that("No open HDF5 objects are left", {
    expect_equal( length(h5validObjects()), 0 )
})

