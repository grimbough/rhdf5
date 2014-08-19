
h5readAttributes <- function(file, name) {
  loc = h5checktypeOrOpenLoc(file, readonly=TRUE)

  if (!H5Lexists(loc$H5Identifier, name)) {
    h5closeitLoc(loc)
    stop("Object ",name," does not exist in this HDF5 file.")
  } else {
    oid = H5Oopen(loc$H5Identifier, name)
    type = H5Iget_type(oid)
    num_attrs = H5Oget_num_attrs(oid)
    if (is.na(num_attrs)) { num_attrs = 0 }
    H5Oclose(oid)
    res = list()
    if (num_attrs > 0) {
      for (i in seq_len(num_attrs)) {
        A = H5Aopen_by_idx(loc$H5Identifier, n = i-1, objname = name)
        attrname <- H5Aget_name(A)
        res[[attrname]] = H5Aread(A)
        H5Aclose(A)
      }
    }
  }  # !H5Lexists
  h5closeitLoc(loc)

  res
}
