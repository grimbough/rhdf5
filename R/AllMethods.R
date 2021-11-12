
#' @describeIn H5IdComponent Print details of the object to screen.
#' 
#' @export
setMethod("show",signature="H5IdComponent", function(object) {
  res <- .Call("_handleInfo", object@ID, PACKAGE='rhdf5')
  res$type = h5const2String("H5I_TYPE",res$type)
  res$type = substr(res$type,5,1000)
  cat(sprintf("HDF5 %s",res$type),
      if (object@native) "(native)",
      "\n")
  if (res$type == "DATASPACE") {
    res2 <- H5Sget_simple_extent_dims(object)
    res$info = c(res$info,
                 rank = res2$rank,
                 size = paste(res2$size, collapse=" x "),
                 maxsize = paste(res2$maxsize, collapse=" x "))
  }
  if (res$type == "DATASET") {
    s = H5Dget_space(object)
    on.exit(H5Sclose(s))
    res2 <- H5Sget_simple_extent_dims(s)
    res$info = c(res$info,
                 type =  .Call("_getDatatypeName", H5Dget_type(object), PACKAGE='rhdf5'),
                 rank = res2$rank,
                 size = paste(res2$size, collapse=" x "),
                 maxsize = paste(res2$maxsize, collapse=" x "))
  }
  if (res$type == "ATTR") {
    s = H5Aget_space(object)
    on.exit(H5Sclose(s))
    res2 <- H5Sget_simple_extent_dims(s)
    names(res$info)[names(res$info) == "name"] = "objName"
    res$info = c(res$info,
                 attrName = H5Aget_name(object),
                 type = .Call("_getDatatypeName", H5Aget_type(object), PACKAGE='rhdf5'),
                 rank = res2$rank,
                 size = paste(res2$size, collapse=" x "),
                 maxsize = paste(res2$maxsize, collapse=" x "))
  }
  if (res$type == "DATATYPE") {
    res$info = c(res$info,
                 type = .Call("_getDatatypeName", object@ID, PACKAGE='rhdf5'))
  }
  if (!is.null(res$info)) {
    cat(paste(sprintf("%12s %s\n",names(res$info),res$info),collapse=""))
  }
  if (res$type %in% c("FILE","GROUP")) {
    cat("\n")
    x = h5ls(object, recursive=FALSE)
    x = format(x, justify = "left")
    x = x[,-1]
    print(x)
  }
  if (!res$isvalid) {
    cat("\nNo valid identifier.\n")
  }
})


#' @describeIn H5IdComponent Returns a group handle or dataset handle for the
#'   group or dataset `name` in the HDF5 location `h5loc`. `h5loc` can either be a file handle
#'   as returned by [H5Fopen] or a group handle as e.g. returned by `h5f$g1` or
#'   `h5f$'/g1/g2'`.
#'   
#' @param e1 An `H5IdComponent` object representing an H5 file or group.
#' @param e2 Character giving the path to an HDF5 group or dataset relative to `e1`.
#' 
#' @export
setMethod(`&`, signature = c("H5IdComponent", "character"), 
          function(e1, e2) {
            H5Oopen(e1, e2)
          } )

#' @describeIn H5IdComponent Reads the HDF5 object `name` in the HDF5 location `x`. `x` can either be
#'   a file handle as returned by [H5Fopen] or a group handle as e.g. returned by
#'   `h5f$g1` or `h5f$'/g1/g2'`.
#'   
#' @param x An `H5IdComponent` object representing an H5 file or group.
#' @param name Character giving the path to an HDF5 group or dataset relative to `x`.
#'
#' @export
setMethod( `$`, signature = c('H5IdComponent'),
           function(x, name) {
             h5id = x
             isvalid = H5Iis_valid(h5id)
             if (!isvalid) {
               stop("Bad HDF5 ID. File, group, or dataset closed?", call. = FALSE)
             }
             truetype = H5Iget_type(h5id)
             #   par = list(file=h5id, name=name)
             #   if ( .hasSlot(h5id, "par") ) {
             #     par = c(par, h5id@par[names(h5id@par) %in% c("compoundAsDataFrame","read.attributes")])
             #   }
             if (truetype %in% c("H5I_FILE", "H5I_GROUP")) {
               res = h5read(file = h5id, name = name)
             } else {
               stop("The provided H5Identifier is not a location identifier.", 
                    call. = FALSE)
             }
             #   
             #   else {
             #     if (truetype == "H5I_DATASET") {
             #       h5a = H5Aopen(h5obj = h5id, name=)
             #       res = h5readAttributes(h5id, name = name)
             #     } else {
             #       stop("The provided H5Identifier is neither a location identifier nor a dataset identifier.", 
             #            call. = FALSE)
             #     }
             #   }
             res
           } )

#' @describeIn H5IdComponent Writes the assigned object to to the HDF5 file at
#'   location e1. e1 can either be a file handle as returned by [H5Fopen] or a
#'   group handle as e.g. returned by h5f$g1 or h5f$'/g1/g2's. The storage.mode
#'   of the assigned object has to be compatible to the datatype of the HDF5
#'   dataset. The dimension of the assigned object have to be identical the
#'   dimensions of the HDF5 dataset. To create a new HDF5 dataset with specific
#'   properties (e.g. compression level or chunk size), please use the function
#'   [h5createDataset] first.
#'   
#' @export
setMethod(`$<-`, signature = c("H5IdComponent"),
          function(x, name, value) {
            h5id = x
            isvalid = H5Iis_valid(h5id)
            if (!isvalid) {
              stop("Bad HDF5 ID. File, group, or dataset closed?", call. = FALSE)
            }
            truetype = H5Iget_type(h5id)
            if (truetype %in% c("H5I_FILE", "H5I_GROUP")) {
              res = h5write(value, file = h5id, name = name)
            } else {
              stop("The provided H5Identifier is not a location identifier.", 
                   call. = FALSE)
            }
            h5id
          } )


#' @describeIn H5IdComponent Subsetting of an HDF5 dataset. The function reads a
#'   subset of an HDF5 dataset. The given dimensions have to fit the dimensions
#'   of the HDF5 dataset.
#'
#' @param x Object of class `H5IdComponent` representing the HDF5 dataset from
#'   which to extract element(s) or in which to replace element(s).
#' @param i,j,\dots Indices specifying elements to extract or replace. Indices
#'   are \code{numeric} vectors or empty (missing) or \code{NULL}.  Numeric
#'   values are coerced to integer as by \code{\link[base]{as.integer}} (and
#'   hence truncated towards zero).
#' @param drop If `TRUE` the result is coerced to the lowest possible dimension
#'   (see the examples).  This only works for extracting elements, not for the
#'   replacement.  See \code{\link[base]{drop}} for further details.
#'
#' @export
setMethod(`[`, signature = c("H5IdComponent", "ANY", "ANY", "ANY"),
          function(x, i, j, ..., drop = TRUE) {
            h5id = x
            index = as.list(sys.call())[-c(1,2)]
            for (i in seq_along(index)) {
              if (index[[i]] == '') {
                index[i] = list(c())
              }
            }
            if (length(index) == 1) {
              if (is.null(index[[1]])) {
                index = NULL
              }
            }
            isvalid = H5Iis_valid(h5id)
            if (!isvalid) {
              stop("Bad HDF5 ID. Dataset closed?", call. = FALSE)
            }
            truetype = H5Iget_type(h5id)
            if (truetype == "H5I_DATASET") {
              res = h5readDataset(h5dataset = h5id, index = index)
              if (drop) {
                res = drop(res)
              }
            } else {
              stop("The provided H5Identifier is not a dataset identifier and can not be subsetted.", 
                   call. = FALSE)
            }
            res
          } )

#' @describeIn H5IdComponent Subsetting of an HDF5 dataset. The function writes
#'   an R data object to a subset of an HDF5 dataset. The given dimensions have
#'   to fit the dimensions of the HDF5 dataset. The HDF5 dataset has to be
#'   created beforehand, e.g. by [h5createDataset].
#'   
#' @param value Array-like \R object containing value to be inserted into
#' the HDF5 dataset.
#'
#' @export
setMethod(`[<-`, signature = c("H5IdComponent", "ANY","ANY","ANY"),
          function(x, i, j, ..., value) {
            h5id = x
            index = as.list(sys.call())
            index = index[-c(1,2,length(index))]
            for (i in seq_along(index)) {
              if (index[[i]] == '') {
                index[i] = list(c())
              }
            }
            if (length(index) == 1) {
              if (is.null(index[[1]])) {
                index = NULL
              }
            }
            isvalid = H5Iis_valid(h5id)
            if (!isvalid) {
              stop("Bad HDF5 ID. Dataset closed?", call. = FALSE)
            }
            truetype = H5Iget_type(h5id)
            if (truetype == "H5I_DATASET") {
              res = h5writeDatasetHelper(obj=value, h5dataset = h5id, index=index)
            } else {
              stop("The provided H5Identifier is not a dataset identifier and can not be subsetted.", 
                   call. = FALSE)
            }
            h5id
          } )


#' @describeIn H5Ref Print details of the object to screen.
#' 
#' @export
setMethod("show",signature="H5Ref", function(object) {
  
  cat("HDF5 REFERENCE\n")
  cat("Type:", h5const2String("H5R_TYPE", object@type), "\n")
  cat("Length:", length(object))
  
})

setMethod("length",
          signature = "H5Ref",
          definition = function(x) {
            if(h5const2String("H5R_TYPE", x@type) == "H5R_OBJECT") {
              div <- 8L
            } else {
              div <- 12L
            }
            return(length(x@val) / div)
          }
)

#' @export
setMethod(f = "c", 
          signature = "H5Ref", 
          definition = function(x, ...) {
            elements <- list(x, ...) 
            if (length(elements) != 0) { 
              items <- unlist(lapply(
                elements,
                FUN = function(object) {
                  if (inherits(object, "H5Ref")) {
                    return(object@val)
                  } else {
                    stop("All objects must be of class 'H5Ref'")
                  }
                }
              ))
              object <- new("H5Ref", val = items, type = x@type)
            }
            return(object)
          }
)

#' @export
setMethod(f = "[", 
          signature = "H5Ref", 
          definition = function(x, i, j, ..., drop = TRUE)  {
            
            if(h5const2String("H5R_TYPE", x@type) == "H5R_OBJECT") {
              div <- 8L
            } else {
              div <- 12L
            }
            
            i <- as.integer(i)
            idx <- c(vapply(i, function(x) ((x-1L)*div)+(1L:div), FUN.VALUE = integer(length = div)))
            
            object <- new("H5Ref", val = x@val[idx], type = x@type)
            return(object)
          }
)
