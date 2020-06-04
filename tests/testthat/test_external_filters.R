#only run these tests if the rhdf5Filters package is present
if(requireNamespace("rhdf5Filters")) {
    
    library(rhdf5)
    
    h5File <- tempfile(pattern = "ex_save", fileext = ".h5")
    
    ############################################################
    context("Writing Using External Filters")
    ############################################################
    
    fid <- H5Fcreate(h5File)
    sid <- H5Screate_simple(dims = 20, maxdims = 20)
    tid <- rhdf5:::.setDataType(H5type = NULL, storage.mode = "integer")
    
    test_that("BZIP2_filter_writing", {
        expect_silent( dcpl <- H5Pcreate("H5P_DATASET_CREATE") )
        expect_silent( H5Pset_fill_time( dcpl, "H5D_FILL_TIME_ALLOC" ) )
        expect_silent( H5Pset_chunk( dcpl, 20) )
        expect_silent( hdf5Filters::H5Pset_bzip2(dcpl) )
        expect_silent( did <- H5Dcreate(fid, "bzip2", tid, sid, dcpl = dcpl) )
        expect_silent( H5Dwrite(buf = 1:20, h5dataset = did) )
        expect_silent( H5Dclose(did) )
    })
    
    H5Sclose(sid)
    H5Fclose(fid)
    
    ############################################################
    context("Reading Using External Filters")
    ############################################################
    
    test_that("BZIP2_filter_reading", {
        expect_silent( fid <- H5Fopen(h5File) )
        expect_silent( did <- H5Dopen(fid, name = "bzip2") )
        expect_equivalent( H5Dread(did), 1:20)
        ## if compression worked the dataset should be smaller than 80 bytes
        expect_less_than( H5Dget_storage_size(did), 4 * 20 )
        expect_silent( H5Dclose(did) )
        expect_silent( H5Fclose(fid) )
    })
}