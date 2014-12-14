
h5set_extent <- function(file, dataset, dims) {
  loc = h5checktypeOrOpenLoc(file)
  if (is.character(dataset)) {
    if (!H5Lexists(loc$H5Identifier, dataset)) {
      h5closeitLoc(loc)
      stop("Object ",dataset," does not exist in this HDF5 file.")
    } else {
      did = H5Oopen(loc$H5Identifier, dataset)
      type = H5Iget_type(did)
      if (type != "H5I_DATASET") {
        H5Oclose(did)
        h5closeitLoc(loc)
        stop("'dataset' is not a dataset")
      }
      res = H5Dset_extent(did, dims)
      H5Oclose(did)
    }
  } else {
    h5checktype(dataset, "dataset")
    res = H5Dset_extent(dataset, dims)
  }

  h5closeitLoc(loc)
  invisible(res)
}
