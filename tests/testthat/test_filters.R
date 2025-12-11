library(rhdf5)

test_that("Tests for filters work", {
  dcpl <- H5Pcreate(type = "H5P_DATASET_CREATE")

  ## default DCPL has no filters set
  expect_true(nzchar(h5checkFilters(dcpl)) == 0L)

  ## deflate fitlers should always be available
  H5Pset_deflate(dcpl, level = 6)
  expect_true(nzchar(h5checkFilters(dcpl)) == 0L)

  H5Pclose(dcpl)
})

test_that("Missing filters are identified", {
  dcpl <- H5Pcreate(type = "H5P_DATASET_CREATE")

  ## lots of things to mock here!
  local_mocked_bindings(
    H5Pget_nfilters = function(h5plist) 1,
    H5Pall_filters_avail = function(h5plist) 0,
    H5Pget_filter = function(h5plist, index) list(404, "foo_filter"),
    H5Zfilter_avail = function(filter_id) FALSE
  )

  expect_true(grepl(
    pattern = "Missing filters: foo_filter",
    x = h5checkFilters(dcpl)
  ))

  H5Pclose(dcpl)
})
