library(rhdf5)

h5File <- withr::local_tempfile(pattern = "ex_save", fileext = ".h5")
vec <- as.integer(rexp(2000, rate = 1.5))


############################################################
context("Writing Using External Filters")
############################################################

fid <- H5Fcreate(h5File)
sid <- H5Screate_simple(dims = 2000, maxdims = 2000)
tid <- rhdf5:::.setDataType(H5type = NULL, storage.mode = "integer")

test_that("BZIP2 filter works for writing", {
  skip_if_not_installed("rhdf5filters")
  expect_silent(dcpl <- H5Pcreate("H5P_DATASET_CREATE"))
  expect_silent(H5Pset_fill_time(dcpl, "H5D_FILL_TIME_ALLOC"))
  expect_silent(H5Pset_chunk(dcpl, 200))
  expect_silent(H5Pset_bzip2(dcpl))
  expect_silent(did <- H5Dcreate(fid, "bzip2", tid, sid, dcpl = dcpl))
  expect_silent(H5Dwrite(buf = vec, h5dataset = did))
  expect_silent(H5Dclose(did))
  expect_silent(H5Pclose(dcpl))
})

test_that("BLOSC filter works for writing", {
  skip_if_not_installed("rhdf5filters")
  expect_silent(dcpl <- H5Pcreate("H5P_DATASET_CREATE"))
  expect_silent(H5Pset_fill_time(dcpl, "H5D_FILL_TIME_ALLOC"))
  expect_silent(H5Pset_chunk(dcpl, 200))
  expect_silent(H5Pset_blosc(dcpl, tid, method = 1L))
  expect_silent(did <- H5Dcreate(fid, "blosc_lz", tid, sid, dcpl = dcpl))
  expect_silent(H5Dwrite(buf = vec, h5dataset = did))
  expect_silent(H5Dclose(did))
  expect_silent(H5Pclose(dcpl))
})

test_that("LZF filter works for writing", {
  skip_if_not_installed("rhdf5filters", minimum_version = "1.3.4")
  expect_silent(dcpl <- H5Pcreate("H5P_DATASET_CREATE"))
  expect_silent(H5Pset_fill_time(dcpl, "H5D_FILL_TIME_ALLOC"))
  expect_silent(H5Pset_chunk(dcpl, 200))
  expect_silent(H5Pset_lzf(dcpl))
  expect_silent(did <- H5Dcreate(fid, "lzf", tid, sid, dcpl = dcpl))
  expect_silent(H5Dwrite(buf = vec, h5dataset = did))
  expect_silent(H5Dclose(did))
  expect_silent(H5Pclose(dcpl))
})

H5Sclose(sid)
H5Fclose(fid)

############################################################
context("Reading Using External Filters")
############################################################

fid <- H5Fopen(h5File)

test_that("BZIP2 filter works when reading", {
  skip_if_not_installed("rhdf5filters")

  expect_silent(did <- H5Dopen(fid, name = "bzip2"))
  expect_equivalent(H5Dread(did), vec)
  ## if compression worked the dataset should be smaller than 8000 bytes
  expect_lt(H5Dget_storage_size(did), 4 * 2000)
  expect_silent(H5Dclose(did))
})

test_that("BLOSC filter works when reading", {
  skip_if_not_installed("rhdf5filters")

  expect_silent(did <- H5Dopen(fid, name = "blosc_lz"))
  expect_equivalent(H5Dread(did), vec)
  ## if compression worked the dataset should be smaller than 8000 bytes
  expect_lt(H5Dget_storage_size(did), 4 * 2000)
  expect_silent(H5Dclose(did))
})

test_that("LZF filter works when reading", {
  skip_if_not_installed("rhdf5filters", minimum_version = "1.3.4")

  expect_silent(did <- H5Dopen(fid, name = "lzf"))
  expect_equivalent(H5Dread(did), vec)
  ## if compression worked the dataset should be smaller than 8000 bytes
  expect_lt(H5Dget_storage_size(did), 4 * 2000)
  expect_silent(H5Dclose(did))
})

H5Fclose(fid)

vlen <- c("this", "is", "a", "variable", "length", "string", "vector", "!!!!!")

test_that("BLOSC works with variable length types", {
  # https://github.com/Huber-group-EMBL/rhdf5/issues/168#issuecomment-5168440454
  skip_if_not_installed("rhdf5filters")
  h5f <- H5Fcreate(tempfile())
  expect_silent(
    h5createDataset(
      h5f,
      dataset = "blosc_vlen",
      dims = length(vlen),
      storage.mode = "character",
      filter = "BLOSC_ZLIB"
    )
  )
  expect_silent(
    h5write(
      vlen,
      h5f,
      "blosc_vlen"
    )
  )
  expect_silent(
    vlen_read <- h5read(
      h5f,
      "blosc_vlen"
    )
  )
  expect_equal(vlen, vlen_read, check.attributes = FALSE)
  H5Fclose(h5f)
})

test_that("LZF works with variable length types", {
  # https://github.com/Huber-group-EMBL/rhdf5/issues/168
  skip_if_not_installed("rhdf5filters")
  h5f <- H5Fcreate(tempfile())
  expect_silent(
    h5createDataset(
      h5f,
      dataset = "lzf_vlen",
      dims = length(vlen),
      storage.mode = "character",
      filter = "LZF"
    )
  )
  expect_silent(
    h5write(
      vlen,
      h5f,
      "lzf_vlen"
    )
  )
  expect_silent(
    vlen_read <- h5read(
      h5f,
      "lzf_vlen"
    )
  )
  expect_equal(vlen, vlen_read, check.attributes = FALSE)
  H5Fclose(h5f)
})

h5closeAll()
