library(rhdf5)

############################################################
context("Utility functions")
############################################################

test_that("Printing library versions", {

    expect_message( h5version(),
                    regexp = "This is Bioconductor rhdf5 [0-9.]+ linking to C-library HDF5 [0-9.]+")
    
})

test_that("We can list created objects", {
    
    fid <- H5Fcreate(name = tempfile())
        
    expect_is( objects_frame <- h5listIdentifier(), "data.frame" )
    expect_is( valid_objects <- h5validObjects(), "list" )
    
    expect_equal( dim(objects_frame), c(1,2))
    expect_length( valid_objects, 1 )
    
    ## create another objects
    sid <- H5Screate()
    
    expect_equal( dim(h5listIdentifier()), c(2,2))
    expect_length( h5validObjects(), 2 )
    
    ## now close them
    H5Sclose(sid)
    H5Fclose(fid)
    
    expect_equal( dim(h5listIdentifier()), c(0,2))
    expect_length( h5validObjects(), 0 )
})

#test_that("Find location of libray", {
#    
#    expect_output( Rhdf5lib:::pkgconfig(), regexp = "libhdf5" )
#    
#    path <- capture.output( Rhdf5lib:::pkgconfig() )
#    if( Sys.info()[['sysname']] != "Windows" ) {
#        libfile <- gsub(pattern = "^-l", replacement = "", x = path)
#        expect_true( file.exists(libfile) )
#    }
#})

