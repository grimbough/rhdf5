#only run these tests if the rhdf5filters package is present
if(requireNamespace("rhdf5filters", quietly = TRUE)) {
    
    library(rhdf5)
    
    h5File <- tempfile(pattern = "ex_save", fileext = ".h5")
    
    ############################################################
    context("Writing Using External Filters")
    ############################################################
    
    fid <- H5Fcreate(h5File)
    sid <- H5Screate_simple(dims = 2000, maxdims = 2000)
    tid <- rhdf5:::.setDataType(H5type = NULL, storage.mode = "integer")
    
    test_that("BZIP2 filter works for writing", {
        expect_silent( dcpl <- H5Pcreate("H5P_DATASET_CREATE") )
        expect_silent( H5Pset_fill_time( dcpl, "H5D_FILL_TIME_ALLOC" ) )
        expect_silent( H5Pset_chunk( dcpl, 200) )
        expect_silent( H5Pset_bzip2(dcpl) )
        expect_silent( did <- H5Dcreate(fid, "bzip2", tid, sid, dcpl = dcpl) )
        expect_silent( H5Dwrite(buf = 1:2000, h5dataset = did) )
        expect_silent( H5Dclose(did) )
    })
    
    test_that("BLOSC filter works for writing", {
        expect_silent( dcpl <- H5Pcreate("H5P_DATASET_CREATE") )
        expect_silent( H5Pset_fill_time( dcpl, "H5D_FILL_TIME_ALLOC" ) )
        expect_silent( H5Pset_chunk( dcpl, 200) )
        expect_silent( H5Pset_blosc(dcpl, tid, method = 1L) )
        expect_silent( did <- H5Dcreate(fid, "blosc_lz", tid, sid, dcpl = dcpl) )
        expect_silent( H5Dwrite(buf = 1:2000, h5dataset = did) )
        expect_silent( H5Dclose(did) )
    })
    
    H5Sclose(sid)
    H5Fclose(fid)
    
    ############################################################
    context("Reading Using External Filters")
    ############################################################
    
    fid <- H5Fopen(h5File)
    
    test_that("BZIP2 filter works when reading", {
        expect_silent( did <- H5Dopen(fid, name = "bzip2") )
        expect_equivalent( H5Dread(did), 1:2000)
        ## if compression worked the dataset should be smaller than 8000 bytes
        expect_lt( H5Dget_storage_size(did), 4 * 2000 )
        expect_silent( H5Dclose(did) )
    })
    
    test_that("BLOSC filter works when reading", {
        expect_silent( did <- H5Dopen(fid, name = "blosc_lz") )
        expect_equivalent( H5Dread(did), 1:2000)
        ## if compression worked the dataset should be smaller than 8000 bytes
        expect_lt( H5Dget_storage_size(did), 4 * 2000 )
        expect_silent( H5Dclose(did) )
    })
    
    H5Fclose(fid)
}