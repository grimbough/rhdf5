
h5createFile <- function(file) {

    res <- FALSE
    if (is.character(file)) {
        file = normalizePath(file,mustWork = FALSE)
        if (file.exists(file)) {
            message("file '", file, "' already exists.")
        } else {
            h5loc <- H5Fcreate(file)
            if (is(h5loc, "H5IdComponent")) {
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
    loc = h5checktypeOrOpenLoc(file, native = FALSE)
    on.exit(h5closeitLoc(loc))
    
    res <- FALSE
    if (is.character(group)) {
        if (H5Lexists(loc$H5Identifier,group)) {
            message("Can not create group. Object with name '", group, "' already exists.")
        } else {
            gid <- H5Gcreate(loc$H5Identifier, group)
            if (is(gid, "H5IdComponent")) {
                H5Gclose(gid)
                res <- TRUE
            }
        }
    }
    
    res
}

.setDataType <- function(H5type, storage.mode, size) {
  
  if (is.null(H5type)) {
    if (is.character(storage.mode)) {
      tid <- switch(storage.mode[1],
                    double = h5constants$H5T["H5T_IEEE_F64LE"],
                    integer = h5constants$H5T["H5T_STD_I32LE"],
                    integer64 = h5constants$H5T["H5T_STD_I64LE"],
                    logical = h5constants$H5T["H5T_STD_I8LE"],
                    raw = h5constants$H5T["H5T_STD_U8LE"],
                    character = {
                      tid <- H5Tcopy("H5T_C_S1")
                      H5Tset_strpad(tid, strpad = "NULLPAD")
                      if (!is.numeric(size)) {
                        stop("parameter 'size' has to be defined for storage.mode character.")
                      }
                      H5Tset_size(tid, size)
                      tid
                    },
                    { stop("datatype ",storage.mode, " not yet implemented.\n", 
                           "Try 'logical', 'double', 'integer', 'integer64' or 'character'.") } )
    } else {
      stop("Can not create dataset. 'storage.mode' has to be a character.")
    }
  } else {
    tid <- h5checkConstants("H5T", H5type)
  }
  if (is.na(tid)) {
    stop("Can not create dataset. H5type unknown. Check h5const('H5T') for valid types.")
  }
  return(tid)
}

.createDCPL <- function(chunk, dims, level, fillValue, dtype, filter, shuffle = FALSE) {
  
  dcpl <- H5Pcreate("H5P_DATASET_CREATE"); 
  if (length(chunk) > 0) {
    
    chunk_size <- H5Tget_size(dtype) * prod(chunk)
    if(chunk_size > 2^32-1) {
      root_dim <- floor(((2^32-1) / H5Tget_size(dtype))^(1/length(chunk)))
      chunk[ chunk > root_dim ] = root_dim
      message("Current chunk settings will exceed HDF5's 4GB limit.\n", 
              "Automatically adjusting them to: ", paste(chunk, collapse = " x "),
              "\nYou may wish to set these to more appropriate values using the 'chunk' argument.")
    }
    
    if ((prod(dims) > 1000000L) & (all(dims == chunk))) {
      message("You created a large dataset with compression and chunking.\n",
              "The chunk size is equal to the dataset dimensions.\n", 
              "If you want to read subsets of the dataset, you should test",
              "smaller chunk sizes to improve read times.")
    }
    H5Pset_fill_time( dcpl, "H5D_FILL_TIME_ALLOC" )
    H5Pset_chunk( dcpl, chunk)
    
    ## set the selected compression filter
    filter <- toupper(filter)
    if(!filter %in% c("GZIP", "ZLIB", "DEFLATE", 
                      "SZIP",
                      "BZIP2",
                      "BLOSC_BLOSCLZ", "BLOSC_LZ4", "BLOSC_LZ4HC", "BLOSC_SNAPPY", "BLOSC_ZLIB", "BLOSC_ZSTD",
                      "LZF",
                      "NONE")) {
      warning("Filter not found, using default: ZLIB")
      filter <- "ZLIB"
    }
    
    ## only use this shuffle if not using blosc filter
    if(shuffle && !grepl("BLOSC", x = filter)) {
      H5Pset_shuffle(dcpl)
    }
    
    ## set the appropriate filter
    if (filter %in% c("GZIP", "ZLIB", "DEFLATE")) { 
      H5Pset_deflate( dcpl, level = level) 
    } else if(filter == "SZIP") {
      H5Pset_szip(dcpl, 1L, 32L)
    } else if(filter == "BZIP2") {
      H5Pset_bzip2( dcpl, level = level )
    } else if (grepl(pattern = "BLOSC", x = filter)) {
      method <- which(c("BLOSC_BLOSCLZ", "BLOSC_LZ4", "BLOSC_LZ4HC", "BLOSC_SNAPPY", "BLOSC_ZLIB", "BLOSC_ZSTD") == filter)
      H5Pset_blosc(dcpl, method = method, h5tid = dtype, level = level, shuffle = shuffle )
    } else if(filter == "LZF") {
      H5Pset_lzf( dcpl, h5tid = dtype )
    }
    
  }
  
  if(!missing(fillValue)) {
    H5Pset_fill_value(dcpl, fillValue)
  }
  
  ## turn off time stamp
  H5Pset_obj_track_times(dcpl, FALSE)
  
  return(dcpl)
}

h5createDataset <- function(file, dataset, dims, maxdims = dims, 
                            storage.mode = "double", H5type = NULL, size = NULL,
                            chunk = dims, fillValue, 
                            level = 6, filter = "gzip", shuffle = TRUE,
                            native = FALSE) {
  
  loc = h5checktypeOrOpenLoc(file, native = native)
  on.exit( h5closeitLoc(loc) )
  
  dims <- as.numeric(dims)
  maxdims <- as.numeric(maxdims)
  
  res <- FALSE
  if (is.character(dataset)) {
    if (H5Lexists(loc$H5Identifier,dataset)) {
      message("Can not create dataset. Object with name '",dataset,"' already exists.")
    } else {
      if (any(is.na(dims)) | any(is.na(maxdims))) {
        message("Can not create dataset. 'dims' and 'maxdims' have to be numeric.")
      } else {
        if (length(maxdims) != length(dims)) {
          stop('"maxdims" has to have the same rank as "dims".')
        }
        if (any(maxdims != dims) & is.null(chunk)) {
          stop('If "maxdims" is different from "dims", chunking is required.')
        }
        if (any(maxdims != H5Sunlimited() & maxdims < dims)) {
          stop('All non-extensible elements of "maxdims" have to be equal or larger than "dims".')
        }
        if (any(dims < 0)) {
          stop('All elements of "dims" must be non-negative.')
        }
        if ((level > 0) & (is.null(chunk))) {
          warning("Compression (level > 0) requires chunking. Set chunk size to activate compression.")
        }
        if (length(chunk) > 0) {
          chunk[which(chunk == 0)] = 1
        }
        
        ## determine data type
        tid <- .setDataType(H5type, storage.mode, size)
        
        dcpl <- .createDCPL(chunk, dims, level, fillValue, dtype = tid, filter = filter, shuffle = shuffle)
        on.exit(H5Pclose(dcpl), add = TRUE)
        
        ## create dataspace
        sid <- H5Screate_simple(dims, maxdims)
        on.exit(H5Sclose(sid), add = TRUE)
        
        did <- H5Dcreate(loc$H5Identifier, dataset, tid, sid, dcpl = dcpl)
        if (is(did, "H5IdComponent")) {
          if (storage.mode[1] == "logical") {
            x = "logical"
            h5writeAttribute(attr = x, h5obj = did, name = "storage.mode")
          }
          H5Dclose(did)
          res <- TRUE
        }
      }
    }
  } 
  res
}

h5createAttribute <- function(obj, attr, dims, maxdims = dims, file, 
                              storage.mode = "double", H5type = NULL, 
                              size = NULL, cset = c("ASCII", "UTF8"), 
                              native = FALSE) {
    
    obj = h5checktypeOrOpenObj(obj, file, native = native)
    on.exit(h5closeitObj(obj))
    
    res <- FALSE

    if (is.null(dims)) {
      sid <- H5Screate()
    } else if (is.numeric(dims) & is.numeric(maxdims)) {
      sid <- H5Screate_simple(dims, maxdims)
      if (!is(sid, "H5IdComponent")) {
        message("Can not create attribute. 'dims' or 'maxdims' argument invalid.")
      }
    } else {
      stop("Can not create attribute. 'dims' and 'maxdims' have to be numeric.")
    }

    on.exit(H5Sclose(sid), add = TRUE)
    if (is.null(H5type)) {
        if (is.character(storage.mode)) {
            tid <- switch(storage.mode[1],
                          double = h5constants$H5T["H5T_IEEE_F64LE"],
                          integer = h5constants$H5T["H5T_STD_I32LE"],
                          character = {
                              tid <- H5Tcopy("H5T_C_S1")
                              H5Tset_cset(tid, match.arg(cset))
                              if (!is.null(size) && !is.numeric(size)) {
                                stop("'size' should be NULL or a number when 'storage.mode=\"character\"'")
                              }
                              H5Tset_size(tid, size) # NULL = variable.
                              tid
                          },
                          { stop("datatype ",storage.mode," not yet implemented. Try 'double', 'integer', or 'character'.") } )
        } else {
            stop("Can not create dataset. 'storage.mode' has to be a character.")
        }
    } else {
        tid <- h5checkConstants("H5T", H5type)
    }
    if(!grepl(pattern = "^[[:digit:]]+$", tid)) {
        message("Can not create attribute. H5type unknown. Check h5const('H5T') for valid types.")
    } else {
        if (H5Aexists(obj$H5Identifier,attr)) {
            message("Can not create attribute. Attribute with name '",attr,"' already exists.")
        } else {
            aid <- H5Acreate(obj$H5Identifier, attr, tid, sid)
            if (is(aid, "H5IdComponent")) {
                H5Aclose(aid)
                res <- TRUE
            }
        }
    }

    res
}

