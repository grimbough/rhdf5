
h5createFile <- function(file) {
  res <- FALSE
  if (is.character(file)) {
    if (file.exists(file)) {
      message("file '", file, "' already exists.")
    } else {
      h5loc <- H5Fcreate(file)
      if (is(h5loc, "H5file")) {
        H5Fclose(h5loc)
        res <- TRUE
      }
    }
  } else {
    stop("file has to be a valid filename.")
  }

  res
}

h5createGroup <- function(file, group) {
  if (is( file, "H5file" ) | is( file, "H5group" )) {
    h5loc = file
  } else {
    if (is.character(file)) {
      if (file.exists(file)) {
        h5loc <- H5Fopen(file)
        if (!is(h5loc, "H5file")) {
          stop("file '",file,"' is not a valid HDF5 file.")
        }
      } else {
        stop("Can not open file '",file,"'.")
      }
    } else {
      stop("file has to be either a valid file or an object of class H5file or H5group.")
    }
  }

  res <- FALSE
  if (is.character(group)) {
    if (H5Lexists(h5loc,group)) {
      message("Can not create group. Object with name '",group,"' already exists.")
    } else {
      gid <- H5Gcreate(h5loc, group)
      if (is(gid, "H5group")) {
        H5Gclose(gid)
        res <- TRUE
      }
    }
  }

  if (!is( file, "H5file" ) & !is( file, "H5group" )) {
    H5Fclose(h5loc)
  }
  res
}

h5createDataset <- function(file, dataset, dims, maxdims = dims, storage.mode = "double", H5type = NULL, size=NULL, chunk=NULL, level=6) {
  if (is( file, "H5file" ) | is( file, "H5group" )) {
    h5loc = file
  } else {
    if (is.character(file)) {
      if (file.exists(file)) {
        h5loc <- H5Fopen(file)
        if (!is(h5loc, "H5file")) {
          stop("file '",file,"' is not a valid HDF5 file.")
        }
      } else {
        stop("Can not open file '",file,"'.")
      }
    } else {
      stop("File has to be either a valid file or an object of class H5file or H5group.")
    }
  }

  res <- FALSE
  if (is.character(dataset)) {
    if (H5Lexists(h5loc,dataset)) {
      message("Can not create dataset. Object with name '",dataset,"' already exists.")
    } else {
      if (is.numeric(dims) & is.numeric(maxdims)) {
        sid <- H5Screate_simple(rev(dims), rev(maxdims))
        if (!is(sid, "H5space")) {
          message("Can not create dataset. 'dims' or 'maxdims' argument invalid.")
        } else {
          if (is.null(H5type)) {
            if (is.character(storage.mode)) {
              tid <- switch(storage.mode[1],
                            double = h5constants$H5T["H5T_NATIVE_DOUBLE"],
                            integer = h5constants$H5T["H5T_NATIVE_INT32"],
                            character = {
                              tid <- H5Tcopy("H5T_C_S1")
                              if (!is.numeric(size)) {
                                stop("parameter '",size,"' has to be defined for storage.mode character.")
                              }
                              H5Tset_size(tid, size)
                              tid
                            },
                            { stop("datatype ",storage.mode," not yet implemented. Try 'double', 'integer', or 'character'.") } )
            } else {
              stop("Can not create dataset. 'storage.mode' has to be a character.")
            }
          } else {
            tid <- h5checkConstants("H5T", H5type)
          }
          if (!is.numeric(tid)) {
            message("Can not create dataset. H5type unknown. Check h5const('H5T') for valid types.")
          } else {
            did <- H5Dcreate(h5loc, dataset, tid, sid, chunk, level)
            if (is(did, "H5dataset")) {
              H5Dclose(did)
              res <- TRUE
            }
          }
          H5Sclose(sid)
        }
      } else {
        message("Can not create dataset. 'dims' and 'maxdims' have to be numeric.")
      }
    }
  }
  
  if (!is( file, "H5file" ) & !is( file, "H5group" )) {
    H5Fclose(h5loc)
  }
  res
}

h5createAttribute <- function(obj, attr, dims, maxdims = dims, file, storage.mode = "double", H5type = NULL, size=NULL) {
  res <- FALSE
  closeFile <- FALSE
  closeGroup <- FALSE
  closeDataset <- FALSE
  if (is.character(obj)) {
    if (missing(file)) {
      stop("If the object is specified by name, you have to specify the file.")
    }
    if (is( file, "H5file" ) | is( file, "H5group" )) {
      h5loc = file
    } else {
      if (is.character(file)) {
        if (file.exists(file)) {
          h5loc <- H5Fopen(file)
          if (!is(h5loc, "H5file")) {
            stop("file '",file,"' is not a valid HDF5 file.")
          }
          closeFile <- TRUE
        } else {
          stop("Can not open file '",file,"'.")
        }
      } else {
        stop("File has to be either a valid file or an object of class H5file or H5group.")
      }
    }
    if (!H5Lexists(h5loc, obj)) {
      message("Object ",obj," not found in file.")
    } else {
      type = H5Lget_info(h5loc, obj)$type
      if (type == "H5O_TYPE_GROUP") {
        obj = H5Gopen(h5loc, obj)
        closeGroup <- TRUE
      } else {
        if (type == "H5O_TYPE_DATASET") {
          obj = H5Dopen(h5loc, obj)
          closeDataset <- TRUE
        } else {
          message("Can not create attribute. Setting attributes for this type of link not supported.")
        }
      }
    }
  } else {
    if (!is( obj, "H5file" ) & !is( obj, "H5group" ) & !is( obj, "H5dataset" )) {
      stop("object ",obj," has to be either a character name, or it has to be of class H5file, H5group, or H5dataset.")
    }
  }

  if (is.numeric(dims) & is.numeric(maxdims)) {
    sid <- H5Screate_simple(rev(dims), rev(maxdims))
    if (!is(sid, "H5space")) {
      message("Can not create attribute. 'dims' or 'maxdims' argument invalid.")
    } else {
      if (is.null(H5type)) {
        if (is.character(storage.mode)) {
          tid <- switch(storage.mode[1],
                        double = h5constants$H5T["H5T_NATIVE_DOUBLE"],
                        integer = h5constants$H5T["H5T_NATIVE_INT32"],
                        character = {
                          tid <- H5Tcopy("H5T_C_S1")
                          if (!is.numeric(size)) {
                            stop("parameter '",size,"' has to be defined for storage.mode character.")
                          }
                          H5Tset_size(tid, size)
                          tid
                        },
                        { stop("datatype ",storage.mode," not yet implemented. Try 'double', 'integer', or 'character'.") } )
        } else {
          stop("Can not create dataset. 'storage.mode' has to be a character.")
        }
      } else {
        tid <- h5checkConstants("H5T", H5type)
      }
      if (!is.numeric(tid)) {
        message("Can not create attribute. H5type unknown. Check h5const('H5T') for valid types.")
      } else {
        if (H5Aexists(obj,attr)) {
          message("Can not create attribute. Attribute with name '",attr,"' already exists.")
        } else {
          aid <- H5Acreate(obj, attr, tid, sid)
          if (is(aid, "H5attribute")) {
            H5Aclose(aid)
            res <- TRUE
          }
        }
      }
      H5Sclose(sid)
    }
  } else {
    stop("Can not create attribute. 'dims' and 'maxdims' have to be numeric.")
  }

  if (closeDataset) { H5Dclose(obj) }
  if (closeGroup) { H5Gclose(obj) }
  if (closeFile) { H5Fclose(h5loc) }
  res
}

