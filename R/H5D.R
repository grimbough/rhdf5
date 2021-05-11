#' Create a new HDF5 dataset
#' 
#' @field h5loc An object of class [H5IdComponent] representing a H5
#' location identifier (file or group). See [H5Fcreate()], [H5Fopen()], 
#' [H5Gcreate()], [H5Gopen()] to create an object of this kind.
#' @field name Name of the dataset.
#' @field dtype_id A character name of a datatype. See `h5const("H5T")`
#' for possible datatypes. Can also be an integer representing an HDF5 datatype.
#' @field h5space An object of class [H5IdComponent] representing a H5 dataspace. 
#' See [H5Dget_space()], [H5Screate_simple()], [H5Screate()] to create an object 
#' of this kind
#' @field lcpl An object of class [H5IdComponent] representing a H5 link creation property list.
#' @field dcpl An object of class [H5IdComponent] representing a H5 dataset creation property list.
#' @field dapl An object of class [H5IdComponent] representing a H5 dataset access property list.
#' 
#' @return An object of class `H5IdComponent` representing the opened dataset.
#' 
#' @export
H5Dcreate <- function( h5loc, name, dtype_id, h5space, lcpl=NULL, dcpl=NULL, dapl=NULL ) {
  h5checktype(h5loc, "loc")
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
  ## dont check if we have an H5T identifier already    
  if (!grepl(pattern = "^[[:digit:]]+$", dtype_id)) {
    dtype_id<- h5checkConstants( "H5T", dtype_id)
  }
  h5checktype(h5space, "dataspace")
  lcpl = h5checktypeAndPLC(lcpl, "H5P_LINK_CREATE", allowNULL = TRUE)
  dcpl = h5checktypeAndPLC(dcpl, "H5P_DATASET_CREATE", allowNULL = TRUE)
  dapl = h5checktypeAndPLC(dapl, "H5P_DATASET_ACCESS", allowNULL = TRUE)
  did <- .Call("_H5Dcreate", h5loc@ID, name, dtype_id, h5space@ID, lcpl@ID, dcpl@ID, dapl@ID, PACKAGE='rhdf5')
  if (did > 0) {
    h5dataset = new("H5IdComponent", ID = did, native = h5loc@native)
  } else {
    message("HDF5: unable to create dataset")
    h5dataset = FALSE
  }
  invisible(h5dataset)
}

#' Open an existing HDF5 dataset
#' 
#' @field h5loc An object of class [H5IdComponent] representing a H5
#' location identifier (file or group).
#' @field name Name of the dataset to open.
#' @field dapl An object of class [H5IdComponent] representing a H5 dataset access property list.
#' 
#' @return An object of class `H5IdComponent` representing the opened dataset.  
#' To prevent memory leaks this must be closed with a call to [H5Dclose()] 
#' when no longer needed.
#' 
#' @examples 
#' h5file <- tempfile(fileext = ".h5")
#' h5createFile( h5file )
#' h5createDataset( h5file, dataset = "A", dims = 10)
#' 
#' fid <- H5Fopen( h5file )
#' did <- H5Dopen( h5loc = fid, name = "A")
#' did
#' 
#' ## rember to close open handles
#' H5Dclose( did )
#' H5Fclose( fid )
#' 
#' @export
H5Dopen <- function( h5loc, name, dapl = NULL ) {
  h5checktype(h5loc, "loc")
  if (length(name)!=1 || !is.character(name)) stop("'filename' must be a character string of length 1")
  dapl = h5checktypeAndPLC(dapl, "H5P_DATASET_ACCESS", allowNULL = TRUE)
  did <- .Call("_H5Dopen", h5loc@ID, name, dapl@ID, PACKAGE='rhdf5')
  if (as.numeric(did) > 0) {
    h5dataset = new("H5IdComponent", ID = did, native = h5loc@native)
  } else {
    message("HDF5: unable to open dataset")
    h5dataset = FALSE
  }
  invisible(h5dataset)
}

#' Close an open HDF5 dataset
#' 
#' @field h5dataset Object of class [H5IdComponent] representing an open HDF5 
#' dataset
#' 
#' @export
H5Dclose <- function( h5dataset ) {
  h5checktype(h5dataset, "dataset")
  invisible(.Call("_H5Dclose", h5dataset@ID, PACKAGE='rhdf5'))
}

H5Dget_type <- function( h5dataset ) {
  h5checktype(h5dataset, "dataset")
  tid <- .Call("_H5Dget_type", h5dataset@ID, PACKAGE='rhdf5')
  invisible(tid)
}

H5Dget_create_plist <- function( h5dataset ) {
  h5checktype(h5dataset, "dataset")
  pid <- .Call("_H5Dget_create_plist", h5dataset@ID, PACKAGE='rhdf5')
  if (pid > 0) {
    h5plist = new("H5IdComponent", ID = pid, native = h5dataset@native)
  } else {
    message("HDF5: unable to create property list")
    h5plist = FALSE
  }
  invisible(h5plist)
}

H5Dget_space <- function( h5dataset ) {
  h5checktype(h5dataset, "dataset")
  sid <- .Call("_H5Dget_space", h5dataset@ID, PACKAGE='rhdf5')
  if (sid > 0) {
    h5space = new("H5IdComponent", ID = sid, native = h5dataset@native)
  } else {
    message("HDF5: unable to create simple data space")
    h5space = FALSE
  }
  invisible(h5space)
}

H5Dget_storage_size <- function( h5dataset ) {
  h5checktype(h5dataset, "dataset")
  size <- .Call("_H5Dget_storage_size", h5dataset@ID, PACKAGE='rhdf5')
  return(size)
}

.postProcessDataSet <- function(h5dataset, res) {
  
  ## handle NA values in logical dataset
  if (H5Aexists(h5obj=h5dataset, name="storage.mode")) {
    att <- H5Aopen(h5obj=h5dataset, name="storage.mode")
    on.exit(H5Aclose(att))
    if (H5Aread(h5attribute = att) == "logical") {
      na_idx <- which(res == -128)
      if(any(na_idx)) {
        res[na_idx] <- NA_integer_
      }
      storage.mode(res) = "logical"
    }
  }
    
  ## warn about NA conversion for integers if 'rhdf5.NA-OK' is missing
  if(!H5Aexists(h5obj = h5dataset, name = "rhdf5-NA.OK")) {
    if( (storage.mode(res) == "integer" || is(res, "integer64")) && any(is.na(res))) {
      if(storage.mode(res) == "integer") {
        na_val <- "-2^31"
      } else if (is(res, "integer64")) {
        na_val <- "-2^63"
      } else { ## we should never end up here
        na_val <- "DEFAULT"
      }
      message("The value ", na_val, " was detected in the dataset.\n",
              "This has been converted to NA within R.")
    }
  }

  res
}

#' Read from an HDF5 dataset
#' 
#' `H5Dread()` reads a (partial) dataset from an HDF5 file into the R session.
#' 
#' Internally, R does not support 64-bit integers. All integers in R are 32-bit
#' integers. By setting bit64conversion='int', a coercing to 32-bit integers is
#' enforced, with the risk of data loss, but with the insurance that numbers 
#' are represented as integers. bit64conversion='double' coerces the 64-bit 
#' integers to floating point numbers. doubles can represent integers with up 
#' to 54-bits, but they are not represented as integer values anymore. For 
#' larger numbers there is again a data loss. bit64conversion='bit64' is 
#' recommended way of coercing. It represents the 64-bit integers as objects 
#' of class 'integer64' as defined in the package 'bit64'. Make sure that you 
#' have installed 'bit64'. The datatype 'integer64' is not part of base R, but 
#' defined in an external package. This can produce unexpected behaviour when 
#' working with the data.
#' 
#' @field h5dataset Object of class [H5IdComponent] representing an open HDF5 
#' dataset.
#' @field h5spaceFile An object of class `H5IdComponent` representing a HDF5 
#' dataspace. See [H5Dget_space()], [H5Screate_simple()], [H5Screate()] to 
#' create an object of this kind.
#' @field h5spaceMem An object of class `H5IdComponent` representing a HDF5 
#' dataspace. See [H5Dget_space()], [H5Screate_simple()], [H5Screate()] to 
#' create an object of this kind. The dimensions of the dataset in the file and
#' in memory. The dimensions in file and in memory are interpreted in an R-like
#' manner. The first dimension is the fastest changing dimension. When reading 
#' the file with a C-program (e.g. HDFView) the order of dimensions will invert, 
#' because in C the fastest changing dimension is the last one.
#' @field buf Buffer to hold the read data.  The buffer size has to fit the 
#' size of the memory space `h5spaceMem`. No extra memory will be allocated for
#' the data. A pointer to the same data is returned.
#' @field compoundAsDataFrame Logical vector of length 1. If `TRUE`, a compound 
#' datatype will be coerced to a `data.frame`. This is not possible, if the 
#' dataset is multi-dimensional. Otherwise the compound datatype will be 
#' returned as a `list`. Nested compound data types will be returned as a 
#' nested `list`.
#' @field bit64conversion Defines how 64-bit integers are converted. (See
#' the details section for more information on these options.)
#' @field drop Logical vector of length 1.  If `TRUE`, the HDF5 object is read 
#' as a vector with NULL dim attributes. Default is `FALSE`.
#' 
#' @export
H5Dread <- function( h5dataset, h5spaceFile=NULL, h5spaceMem=NULL, buf = NULL, compoundAsDataFrame = TRUE,
                     bit64conversion, drop = FALSE ) {
  h5checktype(h5dataset, "dataset")
  h5checktypeOrNULL(h5spaceFile, "dataspace")
  h5checktypeOrNULL(h5spaceMem, "dataspace")
  if (is.null(h5spaceMem)) { sidMem <- NULL } else { sidMem <- h5spaceMem@ID }
  if (is.null(h5spaceFile)) { sidFile <- NULL } else { sidFile <- h5spaceFile@ID }
  if (missing(bit64conversion)) {
    bit64conv = 0L
  } else {
    bit64conv = switch(bit64conversion, int = 0L, double = 1L, bit64 = 2L, default = 0L)
  }
  if (bit64conv == 2L) {
    if (!requireNamespace("bit64",quietly=TRUE)) {
      stop("install package 'bit64' before using bit64conversion='bit64'")
    }
  }
  
  res <- .Call("_H5Dread", h5dataset@ID, sidFile, sidMem, buf, compoundAsDataFrame, 
               bit64conv, drop, h5dataset@native, PACKAGE='rhdf5')
  
  res <- .postProcessDataSet(h5dataset, res)
  res
}

H5Dwrite <- function( h5dataset, buf, h5spaceMem=NULL, h5spaceFile=NULL ) {
  h5checktype(h5dataset, "dataset")
  h5checktypeOrNULL(h5spaceFile, "dataspace")
  h5checktypeOrNULL(h5spaceMem, "dataspace")
  if (is.null(h5spaceMem)) { sidMem <- NULL } else { sidMem <- h5spaceMem@ID }
  if (is.null(h5spaceFile)) { sidFile <- NULL } else { sidFile <- h5spaceFile@ID }
  invisible(.Call("_H5Dwrite", h5dataset@ID, buf, sidFile, sidMem, h5dataset@native, PACKAGE='rhdf5'))
}

#' Change the dimensions of an HDF5 dataset
#' 
#' @details This function can only be applied to datasets that meet the 
#' following criteria:
#' * A chunked dataset with unlimited dimensions
#' * A chunked dataset with fixed dimensions if the new dimension sizes are 
#' less than the maximum sizes set with maxdims
#' 
#' 
#' @field h5dataset
#' @field size
#' 
#' @export
H5Dset_extent <- function( h5dataset, size) {
    h5checktype(h5dataset, "dataset")
    size <- as.numeric(size)
    if (!h5dataset@native) size <- rev(size)
    invisible(.Call("_H5Dset_extent", h5dataset@ID, size, PACKAGE='rhdf5'))
}

#' 
#' @field h5dataset
#' 
#' @export
H5Dchunk_dims <- function(h5dataset) {
    h5checktype(h5dataset, "dataset")
    
    pid <- H5Dget_create_plist(h5dataset)
    on.exit(H5Pclose(pid), add=TRUE)
    
    if (H5Pget_layout(pid) != "H5D_CHUNKED")
        return(NULL)
    else 
        return(rev(H5Pget_chunk(pid)))
}
    