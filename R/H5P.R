
####################################################
## General Property List Operations
####################################################


#' Create a new HDF5 property list
#' 
#' @param type A character name of a property list type. See `h5const("H5P")`
#' for possible property list types.
#' @param native Defunct! Doesn't achieve anything for property lists.
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
## File Creation Properties
####################################################

#' Get version information for objects in a file creation property list
#'
#' @param h5plist [H5IdComponent-class] object representing the file creation property list
#' @returns Named integer vector
#' @export
H5Pget_version <- function(h5plist) {
    h5checktype(h5plist, "plist")
    .Call("_H5Pget_version", h5plist@ID, PACKAGE="rhdf5")
}

#' Get and set the user block size
#' @param h5plist [H5IdComponent-class] object representing the file creation property list
#' @param size of the user block in bytes
#' @rdname H5P_userblock
#' @export
H5Pset_userblock <- function(h5plist, size) {
    h5checktype(h5plist, "plist")
    invisible(.Call("_H5Pset_userblock", h5plist@ID, size, PACKAGE="rhdf5"))
}

#' @rdname H5P_userblock
#' @export
H5Pget_userblock <- function(h5plist) {
    h5checktype(h5plist, "plist")
    .Call("_H5Pget_userblock", h5plist@ID, PACKAGE="rhdf5")
}

#' Get and set the sizes of offsets and lengths used in an HDF5 file
#'
#' @param h5plist [H5IdComponent-class] object representing the file creation property list
#' @param sizeof_addr Offset size in bytes
#' @param sizeof_size Length size in bytes
#' @rdname H5P_sizes
#' @export
H5Pset_sizes <- function(h5plist, sizeof_addr, sizeof_size) {
    h5checktype(h5plist, "plist")
    invisible(.Call("_H5Pset_sizes", h5plist@ID, sizeof_addr, sizeof_size, PACKAGE="rhdf5"))
}

#' @rdname H5P_sizes
#' @export
H5Pget_sizes <- function(h5plist) {
    h5checktype(h5plist, "plist")
    .Call("_H5Pget_sizes", h5plist@ID, PACKAGE="rhdf5")
}

#' Get and set the size of the symbol table B-tree 1/2 rank and the leaf node 1/2 size
#'
#' @param h5plist [H5IdComponent-class] object representing the file creation property list
#' @param ik Symbol table B-tree 1/2 rank
#' @param lk Symbol table leaf node 1/2 size
#' @rdname H5P_sym_k
#' @export
H5Pset_sym_k <- function(h5plist, ik, lk) {
    h5checktype(h5plist, "plist")
    invisible(.Call("_H5Pset_sym_k", h5plist@ID, ik, lk, PACKAGE="rhdf5"))
}

#' @rdname H5P_sym_k
#' @export
H5Pget_sym_k <- function(h5plist) {
    h5checktype(h5plist, "plist")
    .Call("_H5Pget_sym_k", h5plist@ID, PACKAGE="rhdf5")
}

#' Get and set the 1/2 rank of an indexed storage B-tree
#'
#' @param h5plist [H5IdComponent-class] object representing the file creation property list
#' @param ik chunked Storage B-tree 1/2 rank
#' @rdname H5P_istore_k
#' @export
H5Pset_istore_k <- function(h5plist, ik) {
    h5checktype(h5plist, "plist")
    invisible(.Call("_H5Pset_istore_k", h5plist@ID, ik, PACKAGE="rhdf5"))
}

#' @rdname H5P_istore_k
#' @export
H5Pget_istore_k <- function(h5plist) {
    h5checktype(h5plist, "plist")
    .Call("_H5Pget_istore_k", h5plist@ID, PACKAGE="rhdf5")
}

#' Get and set the number of object header message indexes
#'
#' @param h5plist [H5IdComponent-class] object representing the file creation property list
#' @param nindexes Number of shared object header message indexes to be available in files
#' @rdname H5P_shared_mesg_nindexes
#' @export
H5Pset_shared_mesg_nindexes <- function(h5plist, nindexes) {
    h5checktype(h5plist, "plist")
    invisible(.Call("_H5Pset_shared_mesg_nindexes", h5plist@ID, nindexes, PACKAGE="rhdf5"))
}

#' @rdname H5P_shared_mesg_nindexes
#' @export
H5Pget_shared_mesg_nindexes <- function(h5plist) {
    h5checktype(h5plist, "plist")
    .Call("_H5Pget_shared_mesg_nindexes", h5plist@ID, PACKAGE="rhdf5")
}

#' Get and set shared object header message index properties
#'
#' @param h5plist [H5IdComponent-class] object representing the file creation
#'   property list
#' @param index_num Index being configured.  Indices use C-style 0-based counting, so the first index will be numbered 0.
#' @param mesg_type_flags Character specifying the types of messages that may be stored in this index.
#'   Valid values can be found with `h5const(type = "H5O_SHMESG_FLAG")`
#' @param min_mesg_size Minimum message size
#' 
#' @returns `H5Pget_shared_mesg_index()` returns a list of length 2. The first element is the types of messages
#' that may be stored in the index, the second element is the minimum message size.
#' 
#' @rdname H5Pshared_mesg_index
#' @export
H5Pset_shared_mesg_index <- function(h5plist, index_num, 
                                     mesg_type_flags = h5default(type = "H5O_SHMESG_FLAG"), 
                                     min_mesg_size) {
  
    h5checktype(h5plist, "plist")
    mesg_type_flags <- h5checkConstants( "H5O_SHMESG_FLAG", mesg_type_flags )
    invisible(.Call("_H5Pset_shared_mesg_index", h5plist@ID, index_num, 
                    mesg_type_flags, min_mesg_size, PACKAGE="rhdf5"))
}

#' @rdname H5Pshared_mesg_index
#' @export
H5Pget_shared_mesg_index <- function(h5plist, index_num) {
    h5checktype(h5plist, "plist")
    res <- .Call("_H5Pget_shared_mesg_index", h5plist@ID, index_num, PACKAGE="rhdf5")
    return(list(type_flags = h5const2String("H5O_SHMESG_FLAG", res[1]),
                size = res[2]))
}

#' Get and set threshold values for storage of shared object header message indexes
#'
#' @param h5plist [H5IdComponent-class] object representing the file creation property list
#' @param max_list Threshold above which storage shifts from list to B-tree
#' @param min_btree Threshold below which storage reverts to list format
#' @rdname H5P_shared_mesg_phase_change
#' @export
H5Pset_shared_mesg_phase_change <- function(h5plist, max_list, min_btree) {
    h5checktype(h5plist, "plist")
    invisible(.Call("_H5Pset_shared_mesg_phase_change", h5plist@ID, max_list, min_btree, PACKAGE="rhdf5"))
}

#' @rdname H5P_shared_mesg_phase_change
#' @export
H5Pget_shared_mesg_phase_change <- function(h5plist) {
    h5checktype(h5plist, "plist")
    .Call("_H5Pget_shared_mesg_phase_change", h5plist@ID, PACKAGE="rhdf5")
}

####################################################
## File Access Properties
####################################################

#' Set the read-only S3 virtual file driver
#'
#' The read-only S3 virtual file driver can be used to read files hosted
#' remotely on Amazon's S3 storage.
#'
#' @details To access files in a private Amazon S3 bucket you will need to
#'   provide three additional details: The AWS region where the files are
#'   hosted, your AWS access key ID, and your AWS secret access key.  More
#'   information on how to obtain AWS access keys can be found at
#'   \url{https://docs.aws.amazon.com/general/latest/gr/aws-sec-cred-types.html#access-keys-and-secret-access-keys}.
#'   These are provided as a list to the `s3credentials` argument.  If you
#'   are accessing public data this argument should be `NULL`.
#'   
#' @param h5plist [H5IdComponent-class] object representing a file access 
#' property list.
#' @param s3credentials Either \code{NULL} or a list of length 3 specifying the AWS access credentials (see details).
#'
#' @examples
#'
#' ## this doesn't work on the Bioconductor Mac build machine
#' \dontrun{
#' pid <- H5Pcreate("H5P_FILE_ACCESS")
#' H5Pset_fapl_ros3( pid )
#' H5Pclose(pid)
#' }
#' 
#' @export
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

#' Get and set whether to create missing intermediate groups
#'
#' @param h5plist An object of class [H5IdComponent-class] representing a link
#'   creation property list.
#' @param create_groups A logical of length 1 specifying whether missing
#'   groups should be created when a new object is created.  Default is `TRUE`.
#'   
#' @examples  
#' pid <- H5Pcreate("H5P_LINK_CREATE")
#' 
#' ## by default intermediate groups are not created
#' H5Pget_create_intermediate_group( pid )
#' 
#' ## Change the setting so groups will be created
#' 
#' H5Pget_create_intermediate_group( pid )
#' 
#' ## tidy up
#' H5Pclose(pid)
#'
#' @name H5P_create_intermediate_group
NULL

#' @rdname H5P_create_intermediate_group 
#' @export
H5Pset_create_intermediate_group <- function( h5plist, create_groups = TRUE ) {
  
  h5checktypeAndPLC(h5plist, "H5P_LINK_CREATE")
  if(!is.logical(create_groups)) {
    stop("The 'create_groups' argument should be either TRUE or FALSE")
  }
  
  crt_intermed_group = as.integer(create_groups)
  res <- .Call("_H5Pset_create_intermediate_group", h5plist@ID, crt_intermed_group, PACKAGE='rhdf5')
  invisible(res)
}

#' @rdname H5P_create_intermediate_group 
#' @export
H5Pget_create_intermediate_group <- function( h5plist ) {
  h5checktypeAndPLC(h5plist, "H5P_LINK_CREATE")
  res <- .Call("_H5Pget_create_intermediate_group", h5plist@ID, PACKAGE='rhdf5')
  res
}

####################################################
## Dataset Creation Properties
####################################################

#' Get and set the type of storage used to store the raw data for a dataset
#' 
#' Possible options for the `layout` argument are:
#' * `H5D_COMPACT`
#' * `H5D_CONTIGUOUS`
#' * `H5D_CHUNKED`
#' * `H5D_VIRTUAL`
#' 
#' The names of the layout types can also be obtained via `h5const("H5D")`.
#' 
#' @param h5plist An object of class [H5IdComponent-class] representing a dataset creation property list.
#' @param layout A character giving the name of a dataset layout type. 
#' 
#' @name H5P_layout
NULL

#' @rdname H5P_layout
#' @export
H5Pset_layout <- function( h5plist, layout = h5default("H5D") ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  layout <- h5checkConstants( "H5D", layout )
  res <- .Call("_H5Pset_layout", h5plist@ID, layout, PACKAGE='rhdf5')
  invisible(res)
}

#' @rdname H5P_layout
#' @export
H5Pget_layout <- function( h5plist ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  res <- .Call("_H5Pget_layout", h5plist@ID, PACKAGE='rhdf5')
  res <- h5const2String("H5D", res)
  res
}

#' Get and set the size of the chunks used to store a chunked layout dataset
#' 
#' @details Note that a necessary side effect of running this function is that the 
#' layout of the dataset will be changes to `H5D_CHUNKED` if it is not already set to this.
#' 
#' @param h5plist An object of class [H5IdComponent-class] representing a dataset creation property list.
#' @param dim The chunk size used to store the dataset. This argument should be an integer vector of the same length as 
#' the number of dimensions of the dataset the dataset creation property list will be applied to.
#' 
#' @seealso [H5Pset_layout()]
#' 
#' @name H5P_chunk
NULL

#' @rdname H5P_chunk
#' @export
H5Pset_chunk <- function( h5plist, dim ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  if (!is.null(dim) && !h5plist@native) { dim = rev(as.integer(dim)) }
  res <- .Call("_H5Pset_chunk", h5plist@ID, dim, PACKAGE='rhdf5')
  invisible(res)
}

#' @rdname H5P_chunk
#' @export
H5Pget_chunk <- function( h5plist ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  res <- .Call("_H5Pget_chunk", h5plist@ID, PACKAGE='rhdf5')
  res
}

#' Add the deflate compression filter to the chunk processing pipeline.
#'
#' Valid values for the compression level range from 0 (no compression) to 9
#' (best compression, slowest speed). Note that applying this function with
#' `level = 0` does not mean the filter is removed.  It is still part of the
#' filter pipeline, but no compression is performed.  The filter will still need
#' to be available on any system that reads a file created with this setting
#'
#' @param h5plist Object of class [H5IdComponent-class] representing a dataset
#'   creation property list.
#' @param level Integer giving the compression level to use.  Valid values are
#'   from 0 to 9.
#'
#' @export
H5Pset_deflate <- function( h5plist, level ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  level <- as.integer(level)
  res <- .Call("_H5Pset_deflate", h5plist@ID, level, PACKAGE='rhdf5')
  invisible(res)
}

#' Set the fill value for an HDF5 dataset
#' 
#' `H5Pset_fill_value` sets the fill value for a dataset in the dataset creation property list.
#'
#' @param h5plist An object of class [H5IdComponent-class] representing a
#'   dataset creation property list.
#' @param value The default fill value of the dataset. A vector of length 1.
#' 
#' @seealso [H5P_fill_time],[H5Pfill_value_defined]
#'
#' @name H5P_fill_value
NULL

#' @rdname H5P_fill_value
#' @export
H5Pset_fill_value <- function( h5plist, value ) {
  
  if(length(value) > 1L) {
    stop("'value' must be a vector of length 1.")
  }
  
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  storage.mode = storage.mode(value)
  tid <- switch(storage.mode,
                double = h5constants$H5T["H5T_IEEE_F64LE"],
                integer = h5constants$H5T["H5T_STD_I32LE"],
                logical = h5constants$H5T["H5T_STD_I8LE"],
                character = {
                    tid <- H5Tcopy("H5T_C_S1")
                    size <- nchar(value, type = "bytes")
                    H5Tset_size(tid, size)
                    H5Tset_strpad(tid, strpad = "NULLPAD")
                    if(Encoding(value) == "UTF-8") { cset <- "UTF-8" } else { cset <- "ASCII" }
                    H5Tset_cset(tid, cset = cset)
                    tid
                },
                { stop("datatype ",storage.mode," not supported. Try 'double', 'integer', or 'character'.") } )
  res <- .Call("_H5Pset_fill_value", h5plist@ID, tid, value, PACKAGE='rhdf5')
  invisible(res)
}

#' Determine whether a property list has a fill value defined
#'
#' @param h5plist Object of class [H5IdComponent-class] representing a dataset
#'   creation property list.
#'
#' @returns `TRUE` if the fill value is defined, `FALSE` if not.  Will return
#'   `NULL` if there is a problem determining the status of the fill value.
#'
#' @details  Note that the return value for this function is slightly different
#'   from the C version.  The C API provides three return types and can, in the
#'   case that a fill value is defined, differentiate whether the value is the
#'   HDF5 library default or has been set by the application.
#'
#' @export
H5Pfill_value_defined <- function( h5plist ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  res <- .Call("_H5Pfill_value_defined", h5plist@ID, PACKAGE='rhdf5')
  res
}

#' Set the time when fill values are written to a dataset
#'
#' @param h5plist An object of class [H5IdComponent-class] representing a
#'   dataset creation property list.
#' @param fill_time When the fill values should be written.  Possible options
#' can be listed with `h5const("H5D_FILL_TIME")`.
#'
#' @name H5P_fill_time
NULL

#' @rdname H5P_fill_time
#' @export
H5Pset_fill_time <- function( h5plist, fill_time = h5default("H5D_FILL_TIME") ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  fill_time <- h5checkConstants( "H5D_FILL_TIME", fill_time )
  res <- .Call("_H5Pset_fill_time", h5plist@ID, fill_time, PACKAGE='rhdf5')
  invisible(res)
}

#' @rdname H5P_fill_time
#' @export
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

#' Query dataset filter properties.
#'
#' Return information about the filter pipeline applied to a dataset creation
#' property list.
#'
#' * `H5Pall_filters_avail()` checks whether all filters required to process a
#' dataset are available to **rhdf5**.  This can be required if reading files
#' created with other HDF5 software. 
#' * `H5Pget_nfilters()` returns the number of
#' filters in the dataset chunk processing pipeline. 
#' * `H5Pget_filter()`
#' provides details of a specific filter in the pipeline. This includes the
#' filter name and the parameters provided to it e.g. compression level.
#'
#' @param h5plist Object of class [H5IdComponent-class] representing a dataset
#'   creation property list.
#' @param idx Integer of length 1.  This argument selects which filter to return
#'   information about.  Indexing is R-style 1-based.
#'   

#' @rdname H5P_filters
#' @export
H5Pall_filters_avail <- function( h5plist ) {
    h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
    res <- .Call("_H5Pall_filters_avail", h5plist@ID, PACKAGE='rhdf5')
    return(res)
}

#' @rdname H5P_filters
#' @export
H5Pget_nfilters <- function( h5plist ) {
    h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
    res <- .Call("_H5Pget_nfilters", h5plist@ID, PACKAGE='rhdf5')
    res
}

#' @rdname H5P_filters
#' @export
H5Pget_filter <- function( h5plist, idx ) {
    h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
    idx <- as.integer(idx)
    
    if( (idx < 1) || (idx > H5Pget_nfilters(h5plist)) ) {
      stop("'idx' argument is outside the range of filters set on this property list.", 
           call. = FALSE)
    } 
    
    res <- .Call("_H5Pget_filter", h5plist@ID, idx-1L, PACKAGE='rhdf5')
    return(res)
}

#' Add the shuffle filter to the chunk processing pipeline.
#' 
#' @param h5plist Object of class [H5IdComponent-class] representing a dataset
#' creation property list.
#' 
#' @returns Returns (invisibly) an integer vector of length 1.  The only
#'   element of this vector will be non-negative if the filter was set
#'   successfully and negative otherwise.
#' 
#' @export
H5Pset_shuffle <- function( h5plist ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  res <- .Call("_H5Pset_shuffle", h5plist@ID, PACKAGE='rhdf5')
  invisible(res)
}

#' Add the N-Bit filter to the chunk processing pipeline.
#'
#' @param h5plist Object of class [H5IdComponent-class] representing a dataset
#'   creation property list.
#'
#' @returns Returns (invisibly) an integer vector of length 1.  The only
#'   element of this vector will be non-negative if the filter was set
#'   successfully and negative otherwise.
#'
#' @export
H5Pset_nbit <- function( h5plist ) {
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  res <- .Call("_H5Pset_nbit", h5plist@ID, PACKAGE = "rhdf5")
  invisible(res)
}

#' Add the SZIP compression filter to the chunk processing pipeline.
#' 
#' @param h5plist Object of class [H5IdComponent-class] representing a dataset
#' creation property list.
#' @param options_mask,pixels_per_block Integer vectors of length 1, setting parameters 
#' of the SZIP algorithm. See \url{https://portal.hdfgroup.org/display/HDF5/H5P_SET_SZIP} for more details.
#' 
#' @references \url{https://portal.hdfgroup.org/display/HDF5/Szip+Compression+in+HDF+Products}
#' 
#' @export
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

#' Set parameters for the raw data chunk cache
#'
#' @param h5plist Object of class [H5IdComponent-class] representing a dataset
#'   access property list.
#' @param rdcc_nslots Integer defining the number of chunk slots in the raw data
#'   chunk cache for this dataset.
#' @param rdcc_nbytes Integer setting the total size of the raw data chunk cache
#'   for this dataset in bytes. In most cases increasing this number will
#'   improve performance, as long as you have enough free memory. The default
#'   size is 1 MB
#' @param rdcc_w0 Numeric value defining the chunk preemption policy.  Must be between
#'   `0` and `1` inclusive.
#'
#' @name H5P_chunk_cache
NULL

#' @rdname H5P_chunk_cache
#' @export
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
#' @details Objects created using high-level **rhdf5** functions like
#'   [h5createDataset()] will have this setting turned off. This was done to
#'   ensure otherwise identical files returned the same md5 hash. This differs
#'   from the default setting in HDF5, which is for objects to record the times
#'   operations were performed on them.
#'
#' @param h5plist An [H5IdComponent-class] object representing an object
#'   creation property list.
#' @param track_times `logical` specifying whether times associated with an
#'   object should recorded.
#'
#' @name H5Pobject_track_times
NULL

#' @rdname H5Pobject_track_times
#' @export
H5Pset_obj_track_times <- function( h5plist, track_times = TRUE ) {
  
  if(!is.logical(track_times) || is.na(track_times)) {
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



