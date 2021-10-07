#' Create HDF5 file
#' 
#' R function to create an empty HDF5 file.
#' 
#' Creates an empty HDF5 file.
#' 
#' @param file The filename of the HDF5 file.
#' 
#' @return Returns (invisibly) `TRUE` is file was created successfully and `FALSE` otherwise.
#' 
#' @author Bernd Fischer
#' @seealso [h5createGroup()], [h5createDataset()],
#' [h5read()], [h5write()], \link{rhdf5}
#' @examples
#' 
#' h5createFile("ex_createFile.h5")
#' 
#' # create groups
#' h5createGroup("ex_createFile.h5","foo")
#' h5createGroup("ex_createFile.h5","foo/foobaa")
#' 
#' h5ls("ex_createFile.h5")
#' 
#' @name h5_createFile
#' @export h5createFile
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
    
    invisible(res)
}


#' Create HDF5 group
#' 
#' Creates a group within an HDF5 file.
#' 
#' Creates a new group within an HDF5 file.
#' 
#' @param file The filename (character) of the file in which the dataset will
#' be located. For advanced programmers it is possible to provide an object of
#' class [H5IdComponent-class] representing a H5 location identifier
#' (file or group). See [H5Fcreate()], [H5Fopen()],
#' [H5Gcreate()], [H5Gopen()] to create an object of this
#' kind.
#' @param group The name of the new group. The name can contain a hierarchy of
#' groupnames, e.g. `"/group1/group2/newgroup"`, but the function will fail if the
#' top level groups do not exists.
#' 
#' @return Returns TRUE is group was created successfully and FALSE otherwise.
#' @author Bernd Fischer
#' @seealso [h5createFile()], [h5createDataset()],
#' [h5read()], [h5write()]
#' @examples
#' 
#' h5createFile("ex_createGroup.h5")
#' 
#' # create groups
#' h5createGroup("ex_createGroup.h5","foo")
#' h5createGroup("ex_createGroup.h5","foo/foobaa")
#' 
#' h5ls("ex_createGroup.h5")
#' 
#' @name h5_createGroup
#' @export h5createGroup
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

.setDataType <- function(H5type, storage.mode, size, encoding) {
  
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
                      H5Tset_size(tid, size)
                      H5Tset_cset(tid, encoding)
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

    if (!is.null(H5Tget_size(dtype))) { 
        chunk_size <- H5Tget_size(dtype) * prod(chunk)
        if(chunk_size > 2^32-1) {
          root_dim <- floor(((2^32-1) / H5Tget_size(dtype))^(1/length(chunk)))
          chunk[ chunk > root_dim ] = root_dim
          message("Current chunk settings will exceed HDF5's 4GB limit.\n", 
                  "Automatically adjusting them to: ", paste(chunk, collapse = " x "),
                  "\nYou may wish to set these to more appropriate values using the 'chunk' argument.")
        }
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

.checkArgs_createDataset <- function(dims, maxdims, chunk) {
  
  if (any(is.na(dims)) | any(is.na(maxdims))) {
    stop("Can not create dataset. 'dims' and 'maxdims' must be numeric.")
  } 
  if (any(dims < 0)) {
    stop('All elements of "dims" must be non-negative.')
  }
  if (length(maxdims) != length(dims)) {
    stop('"maxdims" has to have the same rank as "dims".')
  } 
  if (any(maxdims != dims) & is.null(chunk)) {
    stop('If "maxdims" is different from "dims", chunking is required.')
  }
  if (any(maxdims != H5Sunlimited() & maxdims < dims)) {
    stop('All non-extensible elements of "maxdims" must be equal or larger than "dims".')
  }
  
  chunk_vs_maxdims <- ((chunk > maxdims) & (maxdims != H5Sunlimited()))
  if(any(chunk_vs_maxdims)) {
    chunk[ which(chunk_vs_maxdims) ]  <- dims[ which(chunk_vs_maxdims) ]
    warning("One or more chunk dimensions exceeded the maximum for the dataset.\n",
            "These have been automatically set to the maximum.\n",
            "The new chunk dimensions are: ", paste0("c(", paste(chunk, collapse = ","), ")"))
  }
  
  return(chunk)
}

#' Create HDF5 dataset
#'
#' R function to create an HDF5 dataset and defining its dimensionality and
#' compression behaviour.
#'
#' Creates a new dataset in an existing HDF5 file. The function will fail if the
#' file doesn't exist or if there exists already another dataset with the same
#' name within the specified file.
#'
#' The `size` argument is only used when `storage.mode = 'character'`.  When
#' storing strings HDF5 can use either a fixed or variable length datatype.
#' Setting `size` to a positive integer will use fixed length strings where
#' `size` defines the length.  \pkg{rhdf5} writes null padded strings by default
#' and so to avoid data loss the value provided here should be the length of the
#' longest string.  Setting `size = NULL` will use variable length strings. The
#' choice is probably dependent on the nature of the strings you're writing. The
#' principle difference is that a dataset of variable length strings will not be
#' compressed by HDF5 but each individual string only uses the space it
#' requires, whereas in a fixed length dataset each string is of length uses
#' `size`, but the whole dataset can be compressed. This explored more in the
#' examples below.
#'
#' The \code{filter} argument can take several options matching to compression
#' filters distributed in either with the HDF5 library in \pkg{Rhdf5lib} or via
#' the \pkg{rhdf5filters} package.  The plugins available and the corresponding
#' values for selecting them are shown below:
#'
#' \describe{ \item{zlib: Ubiquitous deflate compression algorithm used in GZIP
#' or ZIP files.  All three options below achieve the same result.}{ \itemize{
#' \item\code{"GZIP"}, \item\code{"ZLIB"}, \item\code{"DEFLATE"} } } \item{szip:
#' Compression algorithm maintained by the HDF5 group.}{ \itemize{
#' \item\code{"SZIP"} } } \item{bzip2}{ \itemize{ \item\code{"BZIP2"} } }
#' \item{BLOSC meta compressor: As a meta-compressor BLOSC wraps several
#' different compression algorithms.  Each of the options below will active a
#' different compression filter. }{ \itemize{ \item\code{"BLOSC_BLOSCLZ"}
#' \item\code{"BLOSC_LZ4"} \item\code{"BLOSC_LZ4HC"} \item\code{"BLOSC_SNAPPY"}
#' \item\code{"BLOSC_ZLIB"} \item\code{"BLOSC_ZSTD"} } } \item{lzf}{ \itemize{
#' \item\code{"LZF"} } } \item{Disable: It is possible to write chunks without
#' any compression applied.}{ \itemize{ \item\code{"NONE"} } } }
#'
#' @param file The filename (character) of the file in which the dataset will be
#'   located. For advanced programmers it is possible to provide an object of
#'   class [H5IdComponent-class] representing a H5 location identifier (file or
#'   group). See [H5Fcreate()], [H5Fopen()], [H5Gcreate()], [H5Gopen()] to
#'   create an object of this kind.
#' @param dataset Name of the dataset to be created. The name can contain group
#'   names, e.g. 'group/dataset', but the function will fail, if the group does
#'   not yet exist.
#' @param dims The dimensions of the array as they will appear in the file.
#'   Note, the dimensions will appear in inverted order when viewing the file
#'   with a C-programm (e.g. HDFView), because the fastest changing dimension in
#'   R is the first one, whereas the fastest changing dimension in C is the last
#'   one.
#' @param maxdims The maximum extension of the array. Use \code{H5Sunlimited()}
#'   to indicate an extensible dimension.
#' @param storage.mode The storage mode of the data to be written. Can be
#'   obtained by \code{storage.mode(mydata)}.
#' @param H5type Advanced programmers can specify the datatype of the dataset
#'   within the file. See \code{h5const("H5T")} for a list of available
#'   datatypes. If \code{H5type} is specified the argument \code{storage.mode}
#'   is ignored. It is recommended to use \code{storage.mode}
#' @param size For `storage.mode='character'` the maximum string length to use.
#'   The default value of `NULL` will result in using variable length strings.
#'   See the details for more information on this option.
#' @param encoding The encoding of the string data type. Valid options are
#'   "ASCII" or "UTF-8".
#' @param chunk The chunk size used to store the dataset. It is an integer
#'   vector of the same length as \code{dims}. This argument is usually set
#'   together with a compression property (argument \code{level}).
#' @param fillValue Standard value for filling the dataset. The storage.mode of
#'   value has to be convertible to the dataset type by HDF5.
#' @param level The compression level used. An integer value between 0 (no
#'   compression) and 9 (highest and slowest compression).
#' @param filter Character defining which compression filter should be applied
#'   to the chunks of the dataset.  See the Details section for more information
#'   on the options that can be provided here.
#' @param shuffle Logical defining whether the byte-shuffle algorithm should be
#'   applied to data prior to compression.
#' @param native An object of class \code{logical}. If TRUE, array-like objects
#'   are treated as stored in HDF5 row-major rather than R column-major
#'   orientation. Using \code{native = TRUE} increases HDF5 file portability
#'   between programming languages. A file written with \code{native = TRUE}
#'   should also be read with \code{native = TRUE}
#' @return Returns TRUE is dataset was created successfully and FALSE otherwise.
#' @author Bernd Fischer, Mike L. Smith
#' @seealso [h5createFile()], [h5createGroup()], [h5read()], [h5write()]
#' @examples
#'
#' h5createFile("ex_createDataset.h5")
#'
#' # create dataset with compression
#' h5createDataset("ex_createDataset.h5", "A", c(5,8), storage.mode = "integer", chunk=c(5,1), level=6)
#'
#' # create dataset without compression
#' h5createDataset("ex_createDataset.h5", "B", c(5,8), storage.mode = "integer")
#' h5createDataset("ex_createDataset.h5", "C", c(5,8), storage.mode = "double")
#'
#' # create dataset with bzip2 compression
#' h5createDataset("ex_createDataset.h5", "D", c(5,8), storage.mode = "integer",
#'     chunk=c(5,1), filter = "BZIP2", level=6)
#'
#' # create a dataset of strings & define size based on longest string
#' ex_strings <- c('long', 'longer', 'longest')
#' h5createDataset("ex_createDataset.h5", "E",
#'     storage.mode = "character", chunk = 3, level = 6,
#'     dims = length(ex_strings), size = max(nchar(ex_strings)))
#'
#'
#' # write data to dataset
#' h5write(matrix(1:40,nr=5,nc=8), file="ex_createDataset.h5", name="A")
#' # write second column
#' h5write(matrix(1:5,nr=5,nc=1), file="ex_createDataset.h5", name="B", index=list(NULL,2))
#' # write character vector
#' h5write(ex_strings, file = "ex_createDataset.h5", name = "E")
#'
#' h5dump("ex_createDataset.h5")
#'
#' ## Investigating fixed vs variable length string datasets
#'
#' ## create 1000 random strings with length between 50 and 100 characters
#' words <- ceiling(runif(n = 1000, min = 50, max = 100)) |>
#' vapply(FUN = \(x) {
#'  paste(sample(letters, size = x, replace = TRUE), collapse = "")
#' },
#' FUN.VALUE = character(1))
#'
#' ## create two HDF5 files
#' f1 <- tempfile()
#' f2 <- tempfile()
#' h5createFile(f1)
#' h5createFile(f2)
#'
#' ## create two string datasets
#' ## the first is variable length strings, the second fixed at the length of our longest word
#' h5createDataset(f1, "strings", dims = length(words), storage.mode = "character", size = NULL, chunk = 25)
#' h5createDataset(f2, "strings", dims = length(words), storage.mode = "character", size = max(nchar(words)), chunk = 25)
#'
#' ## Write the data
#' h5write(words, f1, "strings")
#' h5write(words, f2, "strings")
#'
#' ## Check file sizes.
#' ## In this example the fixed length string dataset is normally much smaller
#' file.size(f1)
#' file.size(f2)
#'
#' @name h5_createDataset
#' @export h5createDataset
h5createDataset <- function(file, dataset, dims, maxdims = dims, 
                            storage.mode = "double", H5type = NULL, 
                            size = NULL, encoding = c("ASCII", "UTF-8"),
                            chunk = dims, fillValue, 
                            level = 6, filter = "gzip", shuffle = TRUE,
                            native = FALSE) {
  
  loc = h5checktypeOrOpenLoc(file, native = native)
  on.exit( h5closeitLoc(loc) )
  
  dims <- as.numeric(dims)
  maxdims <- as.numeric(maxdims)

  res <- FALSE
  if (!is.character(dataset)) {
    stop('"dataset" argument must be a character vector of length one.')
  }
  if (H5Lexists(loc$H5Identifier,dataset)) {
      message("Can not create dataset. Object with name '",dataset,"' already exists.")
  } 
  chunk <- .checkArgs_createDataset(dims = dims, maxdims = maxdims, chunk = chunk)

  if ((level > 0) & (is.null(chunk))) {
    warning("Compression (level > 0) requires chunking. Set chunk size to activate compression.")
  }
  if (length(chunk) > 0) {
    chunk[which(chunk == 0)] = 1
  }
  
  ## determine data type
  tid <- .setDataType(H5type, storage.mode, size, encoding = match.arg(encoding))
  
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
  res
}

#' Create HDF5 attribute
#' 
#' R function to create an HDF5 attribute and defining its dimensionality.
#' 
#' Creates a new attribute and attaches it to an existing HDF5 object. The
#' function will fail, if the file doesn't exist or if there exists already
#' another attribute with the same name for this object.
#' 
#' You can use [h5writeAttribute()] immediately. It will create the
#' attribute for you.
#' 
#' @param obj The name (character) of the object the attribute will be
#' attatched to. For advanced programmers it is possible to provide an object
#' of class [H5IdComponent-class] representing a H5 object identifier
#' (file, group, dataset). See [H5Fcreate()], [H5Fopen()],
#' [H5Gcreate()], [H5Gopen()], [H5Dcreate()],
#' [H5Dopen()] to create an object of this kind.
#' @param file The filename (character) of the file in which the dataset will
#' be located. For advanced programmers it is possible to provide an object of
#' class [H5IdComponent-class] representing an H5 location identifier. See
#' [H5Fcreate()], [H5Fopen()], [H5Gcreate()],
#' [H5Gopen()] to create an object of this kind. The \code{file}
#' argument is not required, if the argument \code{obj} is of type
#' \code{H5IdComponent}.
#' @param attr Name of the attribute to be created.
#' @param dims The dimensions of the attribute as a numeric vector. If
#' \code{NULL}, a scalar dataspace will be created instead.
#' @param maxdims The maximum extension of the attribute.
#' @param storage.mode The storage mode of the data to be written. Can be
#' obtained by \code{storage.mode(mydata)}.
#' @param H5type Advanced programmers can specify the datatype of the dataset
#' within the file. See \code{h5const("H5T")} for a list of available
#' datatypes. If \code{H5type} is specified the argument \code{storage.mode} is
#' ignored. It is recommended to use \code{storage.mode}
#' @param size The maximum string length when \code{storage.mode='character'}.
#' If this is specified, HDF5 stores each string of \code{attr} as fixed length
#' character arrays. Together with compression, this should be efficient.
#' 
#' If this argument is set to \code{NULL}, HDF5 will instead store
#' variable-length strings.
#' @param cset The encoding to use when \code{storage.mode='character'}.
#' @param native An object of class \code{logical}. If TRUE, array-like objects
#' are treated as stored in HDF5 row-major rather than R column-major
#' orientation. Using \code{native = TRUE} increases HDF5 file portability
#' between programming languages. A file written with \code{native = TRUE}
#' should also be read with \code{native = TRUE}
#' @return Returns TRUE is attribute was created successfully and FALSE
#' otherwise.
#' @author Bernd Fischer
#' @seealso [h5createFile()], [h5createGroup()],
#' [h5createDataset()], [h5read()], [h5write()],
#' \link{rhdf5}
#' @references \url{https://portal.hdfgroup.org/display/HDF5}
#' @keywords programming interface IO file
#' @examples
#' 
#' h5createFile("ex_createAttribute.h5")
#' h5write(1:1, "ex_createAttribute.h5","A")
#' fid <- H5Fopen("ex_createAttribute.h5")
#' did <- H5Dopen(fid, "A")
#' h5createAttribute (did, "time", c(1,10))
#' H5Dclose(did)
#' H5Fclose(fid)
#' 
#' @name h5_createAttribute
#' @export h5createAttribute
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

