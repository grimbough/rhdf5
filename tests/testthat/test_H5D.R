library(rhdf5)

h5File <- tempfile(pattern = "H5D_", fileext = ".h5")

expect_true(h5createFile(h5File))
expect_silent(h5write(matrix(1:200, ncol = 2), file = h5File, name = "foo"))

## The property list interface is really limited at the moment
## so there aren't many functions that we can check
test_that("we can extract the property lists", {
  expect_silent(fid <- H5Fopen(h5File))
  expect_silent(did <- H5Dopen(fid, name = "foo"))

  expect_silent(pid <- H5Dget_create_plist(did))
  expect_output(print(pid), "HDF5 GENPROP_LST")

  expect_silent(H5Pclose(pid))
  expect_silent(H5Dclose(did))
  expect_silent(H5Fclose(fid))
})

test_that("dataset size is reported", {
  expect_silent(fid <- H5Fopen(h5File))
  expect_silent(did <- H5Dopen(fid, name = "foo"))

  expect_silent(dset_size <- H5Dget_storage_size(did))
  expect_is(dset_size, "integer")
  expect_gt(dset_size, 0)

  expect_silent(H5Dclose(did))
  expect_silent(H5Fclose(fid))
})

test_that("we can change the size of a dataset", {
  h5createDataset(
    file = h5File,
    dataset = "/size_changer",
    dims = c(0, 0),
    maxdims = c(H5Sunlimited(), 1000),
    chunk = c(100, 100),
    storage.mode = "integer"
  )

  fid <- H5Fopen(h5File)
  did <- H5Dopen(fid, name = "/size_changer")
  on.exit(h5closeAll(did, fid))

  sid <- H5Dget_space(did)
  expect_identical(H5Sget_simple_extent_dims(sid)$size, c(0L, 0L))
  H5Sclose(sid)

  ## change both i and j a few times
  for (i in seq(100L, 1000L, by = 200L)) {
    for (j in seq(0L, 1000L, by = 500L)) {
      expect_true(H5Dset_extent(did, size = c(i, j)))
      sid <- H5Dget_space(did)
      expect_identical(H5Sget_simple_extent_dims(sid)$size, c(i, j))
      H5Sclose(sid)
    }
  }

  ## can't set outside of maxdim
  expect_false(H5Dset_extent(did, size = c(2000, 2000)))
  ## size remain where we set it last
  sid <- H5Dget_space(did)
  expect_identical(H5Sget_simple_extent_dims(sid)$size, c(i, j))
  H5Sclose(sid)
})

test_that("we can find the number of chunks used by a dataset", {
  ## Create a dataset that will be represented by 4 chunks if complete
  h5createDataset(
    h5File,
    "data",
    dims = c(10, 10),
    chunk = c(5, 5),
    storage.mode = "integer"
  )

  fid <- H5Fopen(h5File)
  did <- H5Dopen(fid, "/data")
  ## Here we return 0 chunks as no values have been written
  expect_identical(H5Dget_num_chunks(did), 0L)

  ## Now write data to half the dataset
  h5writeDataset(
    obj = matrix(1:50, nrow = 10),
    h5loc = fid,
    name = "/data",
    index = list(1:10, 1:5)
  )
  ## We now see it contains 2 chunks
  expect_identical(H5Dget_num_chunks(did), 2L)

  ## Now writing the complte dataset, overwriting the existing values
  h5writeDataset(
    obj = matrix(201:300, ncol = 10),
    h5loc = fid,
    name = "/data",
    index = NULL
  )
  ## We now see it contains 4 chunks
  expect_identical(H5Dget_num_chunks(did), 4L)

  ## Tidy up op handles
  h5closeAll(did, fid)
})
