library(rhdf5)

## output file name
h5File <- system.file("testfiles", "h5ex_t_array.h5", package = "rhdf5")

test_that('timestamps can be read', {
  expect_silent(ts <- h5readTimestamps(h5File, "/DS1"))
  expect_is(ts, 'list')
  expect_length(ts, 4L)
  expect_equal(
    names(ts),
    c('access_time', 'modification_time', 'change_time', 'birth_time')
  )

  expect_is(ts$access_time, 'POSIXct')
  expect_equal(ts$access_time, as.POSIXct(0, tz = 'UTC'))
  expect_equal(ts$change_time, as.POSIXct(1515695505, tz = 'UTC'))
})
