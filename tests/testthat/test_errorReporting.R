library(rhdf5)

############################################################
context("Changing the verbosity of HDF5 error messages")
############################################################

expect_silent(h5errorHandling('suppress'))
expect_message(H5Fopen('foo.h5'), "^HDF5")

expect_silent(h5errorHandling('normal'))
expect_error(H5Fopen('foo.h5'), "HDF5. File accessibilty. Unable to open file.")

expect_silent(h5errorHandling('verbose'))
expect_error(H5Fopen('foo.h5'), "libhdf5\n    error #000:")
