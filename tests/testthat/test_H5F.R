library(rhdf5)

############################################################
context("H5Fcreate")
############################################################

test_that("Invalid file names", {
    expect_error( H5Fcreate(name = 1), regexp = "must be a character string of length 1")
    expect_error( H5Fcreate(name = c("F1", "F2")), regexp = "must be a character string of length 1")
})

h5File <- tempfile(pattern = "H5F_", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

test_that("Default arguments", {
    expect_silent( fid <- H5Fcreate(name = h5File) )
    expect_is(fid , "H5IdComponent" )
    H5Fclose(fid)
})

## this should really be an error
#test_that("Fail if file already open", {
#    fid <- H5Fopen(name = h5File)
#    expect_message( fid2 <- H5Fcreate(name = h5File), 
#                  regexp = "unable to create file")
#    H5Fclose(fid)
#})

test_that("Non-ASCII filename", {

    
    for(name in c("éxample", "år")) {
    
    tmp_dir <- file.path(tempdir(), name)
    dir.create(tmp_dir, showWarnings = FALSE)
    ## this gives a UTF-8 encoded file path on windows
    h5File <- normalizePath(tempfile(pattern = "H5F", fileext = ".h5", tmpdir = tmp_dir), 
                            mustWork = FALSE)
    if(.Platform$OS.type == "windows")
        expect_true(grepl(x = Encoding(h5File), pattern = "UTF-?8"))
    
    fid <- H5Fcreate(h5File)
    expect_true(file.exists(h5File))
    
    if(file.exists(h5File)) {
        H5Fclose(fid)
        file.remove(h5File)
    }
    }
})

############################################################
context("H5Fis_hdf5")
############################################################

h5File <- tempfile(pattern = "H5F_", fileext = ".h5")
if(file.exists(h5File))
    file.remove(h5File)

test_that("Invalid file names", {
    expect_error( H5Fis_hdf5(name = 1), regexp = "must be a character string of length 1")
    expect_error( H5Fis_hdf5(name = c("F1", "F2")), regexp = "must be a character string of length 1")
})

test_that("Missing file", {
    expect_warning(res <- H5Fis_hdf5(name = "/foo/baa.h5"), regexp = "File does not exist.")
    expect_equal(res, NA)
})

test_that("Check if HDF5", {
    
    ## create a new HDF5 file
    if(file.exists(h5File))
        file.remove(h5File)
    expect_silent( fid <- H5Fcreate(name = h5File) )
    H5Fclose(fid)
    ## write a non HDF5 file
    txtFile <- tempfile(fileext = "txt")
    writeLines(text = "foo\nbaa", con = txtFile)
    
    expect_true(H5Fis_hdf5(name = h5File))
    expect_false(H5Fis_hdf5(name = txtFile))
})

############################################################
context("H5F Misc")
############################################################

fid <- H5Fopen(name = h5File)

test_that("H5Fflush doesn't error" , {
    expect_silent( H5Fflush(h5file = fid) )
})

test_that("H5Fget_name" , {
    expect_true( grepl(x = H5Fget_name(h5obj = fid), pattern = basename(h5File) ) )
})

test_that("H5Fget_filesize" , {
    expect_is( H5Fget_filesize(h5file = fid), "numeric" ) %>%
        expect_lt(1000) %>%
        expect_gt(0)
})

H5Fclose(fid)

############################################################
context("H5F plists")
############################################################

test_that("Property list getters", {
    expect_silent(fid <- H5Fopen(name = h5File))
    
    expect_silent(create_plist <- H5Fget_create_plist(fid)) %>%
        expect_is("H5IdComponent") 
    expect_output(show(create_plist), "HDF5 GENPROP_LST")
    expect_silent(H5Pclose(create_plist))
    
    expect_silent(access_plist <- H5Fget_access_plist(fid)) %>%
        expect_is("H5IdComponent")
    expect_output(show(access_plist), "HDF5 GENPROP_LST")
    expect_silent(H5Pclose(access_plist))
    
    expect_silent(H5Fclose(fid))
})

############################################################

test_that("No open HDF5 objects are left", {
    expect_equal( length(h5validObjects()), 0 )
})
