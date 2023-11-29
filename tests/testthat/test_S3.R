
## only run this test if the S3 VFD is configured in Rhdf5lib
## Code here looks for the public config header generated when Rhdf5lib was built
## The functions throw an error if that is not the case and break these tests
run_test <- FALSE
HDF5_conf_file <- system.file("include/H5pubconf.h", package  = "Rhdf5lib")
if(file.exists(HDF5_conf_file)) {
    run_test <- any(grepl(x = readLines(HDF5_conf_file), "H5_HAVE_ROS3_VFD[[:blank:]]+1"))
} 

if(run_test) {

    library(rhdf5)
    
    public_S3_url <- "https://rhdf5-public.s3.eu-central-1.amazonaws.com/h5ex_t_array.h5"
    
    test_that("h5ls() works for files in public S3 buckets", {
        
        skip_on_ci()
        skip_on_bioc()
        skip_if_offline('rhdf5-public.s3.eu-central-1.amazonaws.com')
        
        expect_silent(h5ls_out <- h5ls(public_S3_url, s3 = TRUE))
        expect_is(h5ls_out, "data.frame")
        expect_true( "DS1" %in% h5ls_out$name )
    })
    
    test_that("h5dump() works for files in public S3 buckets", {
        
        skip_on_ci()
        skip_on_bioc()
        skip_if_offline('rhdf5-public.s3.eu-central-1.amazonaws.com')
        
        expect_silent(h5dump_out <- h5dump(public_S3_url, s3 = TRUE))
        expect_is( h5dump_out, "list" )
        expect_equivalent( length(h5dump_out), 1 )
        expect_equivalent( dim(h5dump_out$DS1), c(5,3,4) )
    })
    
    test_that("No open HDF5 objects are left", {
        expect_equal( length(h5validObjects()), 0 )
    })

}
