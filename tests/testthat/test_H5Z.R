library(rhdf5)

test_that("Filters can be tested for", {
  ## deflate should always be available. ID = 1
  expect_true(H5Zfilter_avail(1))
  ## Arbitrary filter ID - hopefully doesn't exist
  expect_false(H5Zfilter_avail(999))
})





