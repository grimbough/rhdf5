test_that("Verbosity of HDF5 error messages can be adjusted", {
  expect_silent(h5errorHandling("suppress"))
  expect_message(H5Fopen("foo.h5"), "^HDF5")

  expect_silent(h5errorHandling("normal"))
  expect_error(
    H5Fopen("foo.h5"),
    "HDF5. File accessibility. Unable to open file.",
    fixed = TRUE
  )

  expect_silent(h5errorHandling("verbose"))
  expect_error(H5Fopen("foo.h5"), "libhdf5\n    error #000:", fixed = TRUE)
})
