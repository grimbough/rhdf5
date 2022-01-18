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
    
    skip_if_not_installed("mockery")
  
    dcpl <- H5Pcreate(type = "H5P_DATASET_CREATE")
    
    ## lots of things to mock here!
    mockery::stub(where = h5checkFilters, 
                  what = "H5Pget_nfilters", 
                  how = 1)
    mockery::stub(where = h5checkFilters, 
                  what = "H5Pall_filters_avail", 
                  how = 0)
    mockery::stub(where = h5checkFilters, 
                  what = "H5Pget_filter", 
                  how = list(404, "foo_filter"))
    mockery::stub(where = h5checkFilters, 
                  what = "H5Zfilter_avail", 
                  how = FALSE)
    
    expect_true(grepl(pattern = "Missing filters: foo_filter",
                      x = h5checkFilters(dcpl)))
    
    H5Pclose(dcpl)
})
