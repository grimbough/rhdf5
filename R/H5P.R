
####################################################
## General Property List Operations
####################################################


#' Create a new HDF5 property list
#' 
#' @param type A character name of a property list type. See `h5const("H5P")`
#' for possible property list types.
#' 
#' @export
H5Pcreate <- function( type = h5default("H5P"), native = FALSE ) {
  type <- h5checkConstants( "H5P", type )
  pid <- .Call("_H5Pcreate", type, PACKAGE='rhdf5')
  if (pid > 0) {
    h5plist = new("H5IdComponent", ID = pid, native = native)
  } else {
    message("HDF5: unable to create property list")
    h5plist = FALSE
  }
  invisible(h5plist)
}

#' Return the property list class identifier for a property list
#' 
#' @param h5plist [H5IdComponent-class] object representing any type of HDF5 
#' property list.
#' 
#' @export
H5Pget_class <- function( h5plist ) {
  h5checktype(h5plist, "plist")
  pclid <- .Call("_H5Pget_class", h5plist@ID, PACKAGE='rhdf5')
  if (pclid > 0) {
    h5plistclass = new("H5IdComponent", ID = pclid, native = h5plist@native)
  } else {
    message("HDF5: unable to get property list class")
    h5plistclass = FALSE
  }
  invisible(h5plistclass)
}

#' Copy an existing property list to create a new property list
#' 
#' @param h5plist [H5IdComponent-class] object representing the property list
#' to be copied.
#' 
#' @export
H5Pcopy <- function( h5plist ) {
  h5checktype(h5plist, "plist")
  pid <- .Call("_H5Pcopy", h5plist@ID, PACKAGE='rhdf5')
  if (pid > 0) {
    h5plistnew = new("H5IdComponent", ID = pid, native = h5plist@native)
  } else {
    message("HDF5: unable to copy property list")
    h5plistnew = FALSE
  }
  invisible(h5plistnew)
}

#' Close and release a property list
#' 
#' `H5Pclose()` terminates access to a property list. All property lists 
#' should be closed when they no longer need to be accessed. This 
#' frees resources used by the property list.  Failing to call `H5Pclose()`
#' can lead to memory leakage over time.
#' 
#' @param h5plist [H5IdComponent-class] object representing the property list
#' to close.
#'  
#' @export
H5Pclose <- function( h5plist ) {
  h5checktype(h5plist, "plist")
  invisible(.Call("_H5Pclose", h5plist@ID, PACKAGE='rhdf5'))
}


####################################################
## File Access Properties
####################################################

#' Set the read-only S3 virtual file driver
#' 
#' The read-only S3 virtual file driver can be used to read files hosted
#' remotely on Amazon's S3 storage.
#' 
#' @keywords Internal
H5Pset_fapl_ros3 <- function( h5plist, s3credentials = NULL ) {
  
    ## this should really check it's a fapl, not just a plist.
    h5checktype(h5plist, "plist")
  
    ## only do authentication if s3credentials are provided
    if(!is.null(s3credentials)) {
      auth <- TRUE
      aws_region <- s3credentials[[1]]
      access_key_id <- s3credentials[[2]]
      secret_access_key <- s3credentials[[3]]
    } else {
      auth <- FALSE
      aws_region <- access_key_id <- secret_access_key <- ""
    }
  
    res <- .Call('_H5Pset_fapl_ros3', h5plist@ID, auth, 
                 aws_region, access_key_id, secret_access_key, 
                 PACKAGE = "rhdf5")
    invisible(res)
}

#' Control the range of HDF5 library versions that will be compatible with
#' a file.
#' 
#' 
#' @param h5plist [H5IdComponent-class] object representing a file access 
#' property list.
#' @param libver_low,libver_high Define the earliest and latest versions of 
#' the HDF5 library that will be used when writing object in the file.
#' 
#' @name H5P_libver_bounds
NULL

#' @rdname H5P_libver_bounds
#' @export
H5Pset_libver_bounds <- function( h5plist, libver_low = "H5F_LIBVER_EARLIEST", libver_high = "H5F_LIBVER_LATEST") {
  h5checktype(h5plist, "plist")
  libver_low <- h5checkConstants( "H5F_LIBVER", libver_low )
  libver_high <- h5checkConstants( "H5F_LIBVER", libver_high )
  res <- .Call("_H5Pset_libver_bounds", h5plist@ID, 
               libver_low, libver_high, PACKAGE='rhdf5')
  invisible(res)
}

#' @rdname H5P_libver_bounds
#' @export
H5Pget_libver_bounds <- function( h5plist ) {
  h5checktype(h5plist, "plist")
  res <- .Call("_H5Pget_libver_bounds", h5plist@ID, PACKAGE='rhdf5')
  res <- h5const2String("H5F_LIBVER", res)
  names(res) = c("libver_low","libver_high")
  res
}

####################################################
## Link Creation Properties
####################################################

H5Pset_char_encoding <- function( h5plist, encoding = h5default("H5T_CSET")) {
  h5checktypeAndPLC(h5plist, "H5P_LINK_CREATE")
  encoding <- h5checkConstants( "H5T_CSET", encoding )
  res <- .Call("_H5Pset_char_encoding", h5plist@ID, encoding, PACKAGE='rhdf5')
  invisible(res)
}

H5Pget_char_encoding <- function( h5plist ) {
  h5checktypeAndPLC(h5plist, "H5P_LINK_CREATE")
  res <- .Call("_H5Pget_char_encoding", h5plist@ID, PACKAGE='rhdf5')
  res <- h5const2String("H5T_CSET", res)
  res
}

H5Pset_create_intermediate_group <- function( h5plist, crt_intermed_group ) {
  h5checktypeAndPLC(h5plist, "H5P_LINK_CREATE")
  crt_intermed_group = as.integer(crt_intermed_group)
  res <- .Call("_H5Pset_create_intermediate_group", h5plist@ID, crt_intermed_group, PACKAGE='rhdf5')
  invisible(res)
}

H5Pget_create_intermediate_group <- function( h5plist ) {
  h5checktypeAndPLC(h5plist, "H5P_LINK_CREATE")
  res <- .Call("_H5Pget_create_intermediate_group", h5plist@ID, PACKAGE='rhdf5')
  res
}

####################################################
## Dataset Creation Properties
####################################################

H5Pset_layout <- function( h5plist, layout = h5default("H5D") ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  layout <- h5checkConstants( "H5D", layout )
  res <- .Call("_H5Pset_layout", h5plist@ID, layout, PACKAGE='rhdf5')
  invisible(res)
}

H5Pget_layout <- function( h5plist ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  res <- .Call("_H5Pget_layout", h5plist@ID, PACKAGE='rhdf5')
  res <- h5const2String("H5D", res)
  res
}

H5Pset_chunk <- function( h5plist, dim ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  if (!is.null(dim) && !h5plist@native) { dim = rev(as.integer(dim)) }
  res <- .Call("_H5Pset_chunk", h5plist@ID, dim, PACKAGE='rhdf5')
  invisible(res)
}

H5Pget_chunk <- function( h5plist ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  res <- .Call("_H5Pget_chunk", h5plist@ID, PACKAGE='rhdf5')
  res
}

H5Pset_deflate <- function( h5plist, level ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  level = as.integer(level)
  res <- .Call("_H5Pset_deflate", h5plist@ID, level, PACKAGE='rhdf5')
  invisible(res)
}

H5Pset_fill_value <- function( h5plist, value ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  storage.mode = storage.mode(value)
  tid <- switch(storage.mode,
                double = h5constants$H5T["H5T_IEEE_F64LE"],
                integer = h5constants$H5T["H5T_STD_I32LE"],
                logical = h5constants$H5T["H5T_STD_I8LE"],
                character = {
                    tid <- H5Tcopy("H5T_C_S1")
                    size <- nchar(value)+1
                    H5Tset_size(tid, size)
                    tid
                },
                { stop("datatype ",storage.mode," not supported. Try 'double', 'integer', or 'character'.") } )
  res <- .Call("_H5Pset_fill_value", h5plist@ID, tid, value, PACKAGE='rhdf5')
  invisible(res)
}

H5Pfill_value_defined <- function( h5plist ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  res <- .Call("_H5Pfill_value_defined", h5plist@ID, PACKAGE='rhdf5')
  res
}

H5Pset_fill_time <- function( h5plist, fill_time = h5default("H5D_FILL_TIME") ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  fill_time <- h5checkConstants( "H5D_FILL_TIME", fill_time )
  res <- .Call("_H5Pset_fill_time", h5plist@ID, fill_time, PACKAGE='rhdf5')
  invisible(res)
}

H5Pget_fill_time <- function( h5plist ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  res <- .Call("_H5Pget_fill_time", h5plist@ID, PACKAGE='rhdf5')
  res <- h5const2String("H5D_FILL_TIME", res)
  res
}

H5Pset_alloc_time <- function( h5plist, alloc_time = h5default("H5D_ALLOC_TIME") ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  alloc_time <- h5checkConstants( "H5D_ALLOC_TIME", alloc_time )
  res <- .Call("_H5Pset_alloc_time", h5plist@ID, alloc_time, PACKAGE='rhdf5')
  invisible(res)
}

H5Pget_alloc_time <- function( h5plist ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  res <- .Call("_H5Pget_alloc_time", h5plist@ID, PACKAGE='rhdf5')
  res <- h5const2String("H5D_ALLOC_TIME", res)
  res
}

H5Pall_filters_avail <- function( h5plist ) {
    h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
    res <- .Call("_H5Pall_filters_avail", h5plist@ID, PACKAGE='rhdf5')
    return(res)
}

H5Pget_nfilters <- function( h5plist ) {
    h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
    res <- .Call("_H5Pget_nfilters", h5plist@ID, PACKAGE='rhdf5')
    res
}

H5Pget_filter <- function( h5plist, idx ) {
    h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
    idx <- as.integer(idx)
    res <- .Call("_H5Pget_filter", h5plist@ID, idx, PACKAGE='rhdf5')
    return(res)
}

H5Pset_shuffle <- function( h5plist ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  res <- .Call("_H5Pset_shuffle", h5plist@ID, PACKAGE='rhdf5')
  invisible(res)
}

H5Pset_szip <- function( h5plist, options_mask, pixels_per_block ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  options_mask = as.integer(options_mask)
  pixels_per_block = as.integer(pixels_per_block)
  res <- .Call("_H5Pset_szip", h5plist@ID, options_mask, pixels_per_block, PACKAGE='rhdf5')
  invisible(res)
}


####################################################
## Dataset Access Properties
####################################################

H5Pset_chunk_cache <- function( h5plist, rdcc_nslots, rdcc_nbytes, rdcc_w0 ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_ACCESS")
  rdcc_nslots = as.integer(rdcc_nslots)
  rdcc_nbytes = as.integer(rdcc_nbytes)
  rdcc_w0 = as.double(rdcc_w0)
  res <- .Call("_H5Pset_chunk_cache", h5plist@ID, rdcc_nslots, rdcc_nbytes, rdcc_w0, PACKAGE='rhdf5')
  invisible(res)
}


####################################################
## Object Creation Properties
####################################################

#' Set whether to record timestamps for operations performed on an HDF5 object.
#' 
#' @details Objects created using high-level
#' **rhdf5** functions like [h5createDataset()] will have this setting turned off.
#' This was done to ensure otherwise identical files returned the same md5 hash. 
#' This differs from the default setting in HDF5, which is for objects to record 
#' the times operations were performed on them.
#' 
#' @param h5plist An [H5IdComponent-class] object representing an object 
#' creation property list.
#' @param `logical` specifying whether times associated with an object should recorded.
#' 
#' @name H5Pobject_track_times
NULL

#' @rdname H5Pobject_track_times
#' @export
H5Pset_obj_track_times <- function( h5plist, track_times = TRUE ) {
  
  if(!is.logical(track_times) | is.na(track_times)) {
    stop("Argument 'track_times' must be either TRUE or FALSE")
  }

   res <- .Call("_H5Pset_obj_track_times", h5plist@ID, as.integer(track_times), PACKAGE='rhdf5')
   invisible(res)
}

#' @rdname H5Pobject_track_times
#' @export
H5Pget_obj_track_times <- function( h5plist ) {
   res <- .Call("_H5Pget_obj_track_times", h5plist@ID, PACKAGE='rhdf5')
   return(res)
}

####################################################
## Generic Property Operations (Advanced)
####################################################

H5Pequal <- function( h5plistclass1, h5plistclass2 ) {
  h5checktype(h5plistclass1, "plistclass")
  h5checktype(h5plistclass2, "plistclass")
  res <- .Call("_H5Pequal", h5plistclass1@ID, h5plistclass2@ID, PACKAGE='rhdf5')
  as.logical(res)
}

H5Pclose_class <- function( h5plistclass ) {
  h5checktype(h5plistclass, "plistclass")
  invisible(.Call("_H5Pclose_class", h5plistclass@ID, PACKAGE='rhdf5'))
}



