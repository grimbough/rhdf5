library(rhdf5)

############################################################
context("H5P functions")
############################################################

test_that("Property list creation & closure", {
  expect_silent(pid <- H5Pcreate())
  expect_is(pid, "H5IdComponent")
  expect_silent(H5Pclose(pid))
})


test_that("Property list creation & closure", {
  expect_silent(pid1 <- H5Pcreate())

  expect_silent(pid2 <- H5Pcopy(pid1))
  expect_is(pid2, "H5IdComponent")

  expect_silent(H5Pclose(pid1))
  expect_silent(H5Pclose(pid2))
})

test_that("setting and getting libhdf5 version bounds", {
  expect_silent(pid1 <- H5Pcreate("H5P_FILE_ACCESS"))

  expect_output(
    default_version_bounds <- H5Pget_libver_bounds(pid1),
    regexp = "^low"
  )
  expect_type(default_version_bounds, "character")

  ## Setters work as expected.
  expect_silent(H5Pset_libver_bounds(
    pid1,
    libver_low = "H5F_LIBVER_V110",
    libver_high = "H5F_LIBVER_LATEST"
  ))
  expect_output(version_bounds <- H5Pget_libver_bounds(pid1), regexp = "^low")
  expect_equivalent(version_bounds, c("H5F_LIBVER_V110", "H5F_LIBVER_LATEST"))

  # Restoring the bounds to their original values.
  expect_silent(H5Pset_libver_bounds(
    pid1,
    libver_low = default_version_bounds[1],
    libver_high = default_version_bounds[2]
  ))
  expect_output(version_bounds <- H5Pget_libver_bounds(pid1), regexp = "^low")
  expect_identical(version_bounds, default_version_bounds)

  expect_silent(H5Pclose(pid1))
})

test_that("Dataset creation properties can be set", {
  expect_silent(pid <- H5Pcreate("H5P_DATASET_CREATE"))

  ## use default layout
  expect_silent(layout <- H5Pget_layout(pid))
  expect_type(layout, "character")
  expect_match(as.character(layout), "H5D_CONTIGUOUS", fixed = TRUE)
  ## change to chunked
  expect_silent(H5Pset_layout(pid, layout = "H5D_CHUNKED"))
  ## check changes
  expect_silent(H5Pget_layout(pid)) |>
    expect_match("H5D_CHUNKED", fixed = TRUE)

  ## seting chunk sizes
  expect_null(H5Pget_chunk(pid))
  H5Pset_chunk(pid, dim = c(100, 100))
  expect_identical(H5Pget_chunk(pid), c(100L, 100L))

  ## default fill values
  ## I think this is defined by default
  expect_true(H5Pfill_value_defined(pid))
  expect_silent(H5Pset_fill_value(pid, 10))
  expect_silent(H5Pset_fill_value(pid, 10L))
  expect_silent(H5Pset_fill_value(pid, "foo"))
  expect_silent(H5Pset_fill_value(pid, TRUE))
  expect_error(H5Pset_fill_value(pid, sum))

  ## setting whether times are tracked
  expect_silent(H5Pset_obj_track_times(pid, TRUE))
  expect_true(H5Pget_obj_track_times(pid))
  expect_silent(H5Pset_obj_track_times(pid, FALSE))
  expect_false(H5Pget_obj_track_times(pid))

  expect_silent(H5Pclose(pid))
})
