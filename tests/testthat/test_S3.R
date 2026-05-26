## only run this test if the S3 VFD is configured in Rhdf5lib
## Code here looks for the public config header generated when Rhdf5lib was built
## The functions throw an error if that is not the case and break these tests
run_test <- FALSE
HDF5_conf_file <- system.file("include/H5pubconf.h", package = "Rhdf5lib")
if (file.exists(HDF5_conf_file)) {
  run_test <- any(grepl(
    x = readLines(HDF5_conf_file),
    "H5_HAVE_ROS3_VFD[[:blank:]]+1"
  ))
}
skip_if_not(run_test, "S3 VFD not configured in Rhdf5lib")

public_S3_url <- "https://rhdf5-public.s3.eu-central-1.amazonaws.com/h5ex_t_array.h5"

test_that("h5ls() works for files in public S3 buckets", {
  skip_on_ci()
  skip_on_bioc()
  skip_if_offline("rhdf5-public.s3.eu-central-1.amazonaws.com")

  expect_silent(h5ls_out <- h5ls(public_S3_url, s3 = TRUE))
  expect_s3_class(h5ls_out, "data.frame")
  expect_in("DS1", h5ls_out$name)
})

test_that("h5dump() works for files in public S3 buckets", {
  skip_on_ci()
  skip_on_bioc()
  skip_if_offline("rhdf5-public.s3.eu-central-1.amazonaws.com")

  expect_silent(h5dump_out <- h5dump(public_S3_url, s3 = TRUE))
  expect_type(h5dump_out, "list")
  expect_length(h5dump_out, 1)
  expect_shape(h5dump_out$DS1, dim = c(5, 3, 4))
})
