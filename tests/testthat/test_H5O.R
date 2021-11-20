library(rhdf5)

h5File <- tempfile(pattern = "H5O_", fileext = ".h5")
h5createFile(h5File)

test_that("orphan objects can be linked into a file", {
    ## open file and create orphan group
    fid <- H5Fopen( h5File )
    gid <- H5Gcreate_anon(fid)
    
    ## insert group into file at /foo
    expect_silent(H5Olink(h5obj = gid, h5loc = fid, newLinkName = "foo"))
    
    H5Gclose(gid)
    H5Fclose(fid)
    
    ## check new group is in the file
    ls_out <- h5ls(h5File)
    expect_identical(nrow(ls_out), 1L)
    expect_identical(ls_out$name, "foo")
})

