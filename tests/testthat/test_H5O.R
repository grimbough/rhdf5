library(rhdf5)

h5File <- tempfile(pattern = "H5O_", fileext = ".h5")
h5File2 <- tempfile(pattern = "H5O_", fileext = ".h5")
h5createFile(h5File)
h5createFile(h5File2)
h5write(1:10, file = h5File, name = "DS1")

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
  expect_identical(nrow(ls_out), 2L)
  expect_identical(ls_out$name, c("DS1", "foo"))
})

test_that("Objects can be copied in the same file", {
  
  fid1 <- H5Fopen( h5File )
  
  expect_silent(H5Ocopy(h5loc = fid1, name = "DS1", h5loc_dest = fid1, name_dest = "DS2"))
  
  H5Fclose(fid1)
  
  ## check new group is in the file
  ls_out <- h5ls(h5File)
  expect_identical(nrow(ls_out), 3L)
  expect_identical(ls_out$name, c("DS1", "DS2", "foo"))
  
})

test_that("Objects can be copied in a different file", {
  
  fid1 <- H5Fopen( h5File )
  fid2 <- H5Fopen( h5File2 )
  
  expect_silent(H5Ocopy(h5loc = fid1, name = "DS1", 
                        h5loc_dest = fid2, name_dest = "DS1"))
  
  ## if we want to create a new group hierarchy we can use a link creation property list
  lcpl <- H5Pcreate("H5P_LINK_CREATE")
  on.exit(H5Pclose(lcpl), add = TRUE)
  H5Pset_create_intermediate_group( lcpl, create_groups = TRUE )
  
  expect_silent(H5Ocopy(h5loc = fid1, name = "DS1", 
                        h5loc_dest = fid2, name_dest = "/foo/baa/DS1_nested", 
                        lcpl = lcpl))
  
  H5Fclose(fid1)
  H5Fclose(fid2)
  
  ## check new group is in the file
  ls_out <- h5ls(h5File2)
  expect_identical(nrow(ls_out), 4L)
  expect_identical(ls_out$name, c("DS1", "foo", "baa", "DS1_nested"))
  
})

