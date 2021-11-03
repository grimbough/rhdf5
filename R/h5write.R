h5writeDatasetHelper <- function (obj, h5dataset, index = NULL, start = NULL, stride = NULL, 
                                  block = NULL, count = NULL)
{
    try({
        h5spaceFile <- H5Dget_space(h5dataset)
        on.exit(H5Sclose(h5spaceFile))
    })
    
    if (!is.null(index)) {
        s = H5Sget_simple_extent_dims(h5spaceFile)$size
        if (length(index) != length(s)) {
            stop("length of index has to be equal to dimensional extension of HDF5 dataset.")
        }
        for (i in seq_len(length(index))) {
            if (is.null(index[[i]])) {
                index[[i]] = seq_len(s[i])
            } else if( is.call(index[[i]] )) {
                index[[i]] <- eval(index[[i]])
            }
        }
        try({
            H5Sselect_index(h5spaceFile, index)
        })
        if(length(index) > 1) {
            ## indexing an array
            d = sapply(index, length)
            d[d == 0] = dim(obj)[d == 0]
            dim(obj) = d
        } 
        I = list()
        for (i in seq_len(length(index))) {
            m <- match(index[[i]], unique(sort(index[[i]])))
            I[[i]] = order(m)
            I[[i]] = I[[i]][!duplicated(m[I[[i]]], fromLast = TRUE)]
        }
        obj <- do.call("[", c(list(obj), I))
    }
    else {
        if (any(c(!is.null(start), !is.null(stride), !is.null(block), 
                  !is.null(count)))) {
            if (is.null(block) & is.null(count)) {
                if(is.null(dim(obj))) {
                    block <- 1
                    count <- 1
                } else {
                    block = rep(1, length(dim(obj)))
                    count = dim(obj)
                }
            }
            try({
                H5Sselect_hyperslab(h5spaceFile, start = start, 
                                    stride = stride, count = count, block = block)
            })
        }
    }
    DimMem <- dim(obj)
    if (is.null(DimMem)) {
        DimMem = length(obj)
    }
    try({
        h5spaceMem <- H5Screate_simple(DimMem, NULL)
        on.exit(H5Sclose(h5spaceMem), add = TRUE, after = FALSE)
    })
    try({
        res <- H5Dwrite(h5dataset, obj, h5spaceMem = h5spaceMem, 
                        h5spaceFile = h5spaceFile)
    })

    invisible(NULL)
}

#' Write object to an HDF5 file.
#'
#' Writes an R object to an HDF5 file. This function can be used to write either
#' full arrays/vectors or subarrays (hyperslabs) within an existing dataset.
#'
#' Writes an R object to an HDF5 file. If none of the arguments \code{start,
#' stride, block, count} is specified, the dataset has the same dimension in the
#' HDF5 file and in memory. If the dataset already exists in the HDF5 file, one
#' can write subarrays, (so called hyperslabs) to the HDF5 file. The arguments
#' \code{start, stride, block, count} define the subset of the dataset in the
#' HDF5 file that is to be written to. See these introductions to hyperslabs:
#' \url{https://support.hdfgroup.org/HDF5/Tutor/selectsimple.html},
#' \url{https://support.hdfgroup.org/HDF5/Tutor/select.html} and
#' \url{http://ftp.hdfgroup.org/HDF5/Tutor/phypecont.html}. Please note that in
#' R the first dimension is the fastest changing dimension.
#'
#' When viewing the HDF5 datasets with any C-program (e.g. HDFView), the order
#' of dimensions is inverted. In the R interface counting starts with 1, whereas
#' in the C-programs (e.g. HDFView) counting starts with 0.
#'
#' @param obj The R object to be written.
#' @param file The filename (character) of the file in which the dataset will be
#'   located. For advanced programmers it is possible to provide an object of
#'   class [H5IdComponent-class] representing a H5 location identifier (file or
#'   group). See \code{\link{H5Fcreate}}, \code{\link{H5Fopen}},
#'   \code{\link{H5Gcreate}}, \code{\link{H5Gopen}} to create an object of this
#'   kind.
#' @param h5loc An object of class [H5IdComponent-class] representing a H5
#'   location identifier (file or group). See \code{\link{H5Fcreate}},
#'   \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}} to
#'   create an object of this kind.
#' @param name The name of the dataset in the HDF5 file.
#' @param index List of indices for subsetting. The length of the list has to
#'   agree with the dimensional extension of the HDF5 array. Each list element
#'   is an integer vector of indices. A list element equal to `NULL` chooses all
#'   indices in this dimension. Counting is R-style 1-based.
#' @param start The start coordinate of a hyperslab (similar to subsetting in
#'   R). Counting is R-style 1-based. This argument is ignored, if index is not
#'   NULL.
#' @param stride The stride of the hypercube. Read the introduction
#'   \url{http://ftp.hdfgroup.org/HDF5/Tutor/phypecont.html} before using this
#'   argument. R behaves like Fortran in this example. This argument is ignored,
#'   if index is not NULL.
#' @param block The block size of the hyperslab. Read the introduction
#'   \url{http://ftp.hdfgroup.org/HDF5/Tutor/phypecont.html} before using this
#'   argument. R behaves like Fortran in this example. This argument is ignored,
#'   if index is not NULL.
#' @param count The number of blocks to be written. This argument is ignored, if
#'   index is not NULL.
#' @param chunk Specifies the number of items to be include in an HDF5 chunk. If
#'   left unspecified the defaults is the smaller of: the total number of
#'   elements or the number of elements that fit within 4GB of memory. If
#'   `DataFrameAsCompound=FALSE` each row of the `data.frame` can be consider an
#'   "element".
#' @param level The compression level. An integer value between 0 (no
#'   compression) and 9 (highest and slowest compression). Only used, if the
#'   dataset does not yet exist. See [h5createDataset()] to create an dataset.
#' @param native An object of class \code{logical}. If TRUE, array-like objects
#'   are treated as stored in HDF5 row-major rather than R column-major
#'   orientation. Using \code{native = TRUE} increases HDF5 file portability
#'   between programming languages. A file written with \code{native = TRUE}
#'   should also be read with \code{native = TRUE}
#' @param DataFrameAsCompound If true, a `data.frame` will be saved as a
#'   compound data type. Otherwise it is saved like a list. The advantage of
#'   saving a data.frame as a compound data type is that it can be read as a
#'   table from python or with a struct-type from C. The disadvantage is that
#'   the data has to be rearranged on disk and thus can slow down I/O. If fast
#'   reading is required, `DataFrameAsCompound=FALSE` is recommended.
#' @param size The length of the fixed-width string data type, when `obj` is a
#'   character vector. If `NULL`, this is set to the length of the largest
#'   string.
#' @param variableLengthString Whether character vectors should be written as
#'   variable-length strings into the attributes. If `TRUE`, `size` is ignored.
#' @param encoding The encoding of the string data type.  Valid options are
#'   "ASCII" or "UTF-8".
#' @param createnewfile If `TRUE`, a new file will be created if necessary.
#' @param write.attributes (logical) If TRUE, all R-attributes attached to the
#'   object \code{obj} are written to the HDF5 file.
#' @param \dots Further arguments passed to \code{\link{H5Dwrite}}.
#'
#' @return \code{h5write} returns 0 if successful.
#'
#' @author Bernd Fischer, Mike Smith
#'
#' @seealso \code{\link{h5ls}}, \code{\link{h5createFile}},
#'   \code{\link{h5createDataset}}, \link{rhdf5}
#' @references \url{https://portal.hdfgroup.org/display/HDF5}
#' @keywords programming interface IO file
#' @examples
#'
#' h5File <- tempfile(fileext = ".h5")
#' h5createFile( h5File )
#'
#' # write a matrix
#' B = array(seq(0.1,2.0,by=0.1),dim=c(5,2,2))
#' attr(B, "scale") <- "liter"
#' h5write(B, h5File,"B")
#'
#' # write a submatrix
#' h5createDataset(h5File, "S", c(5,8), storage.mode = "integer", chunk=c(5,1), level=7)
#' h5write(matrix(1:5,nr=5,nc=1), file=h5File, name="S", index=list(NULL,1))
#'
#' @name h5_write
#' @export h5write
h5write <- function(obj, file, name, ...) {
    res <- UseMethod("h5write")
    invisible(res)
}

#' @rdname h5_write
#' @export h5write.default
h5write.default <- function(obj, file, name, createnewfile=TRUE, write.attributes = FALSE, ..., native = FALSE) {
    loc = h5checktypeOrOpenLoc(file, createnewfile = createnewfile, native = native)
    on.exit(h5closeitLoc(loc))
    
    res <- h5writeDataset(obj, loc$H5Identifier, name, ...)
    if (write.attributes) {
        oid = H5Oopen(loc$H5Identifier, name)
        type = H5Iget_type(oid)
        H5Oclose(oid)
        
        if (type == "H5I_GROUP") {
            h5obj = H5Gopen(loc$H5Identifier, name)
            on.exit(H5Gclose(h5obj), add = TRUE)
        } else {
            if (type == "H5I_DATASET") {
                h5obj = H5Dopen(loc$H5Identifier, name)
                on.exit(H5Dclose(h5obj), add = TRUE)
            } else {
                stop("Cannot open object of this type")
            }
        }
        Attr <- attributes(obj)
        for (i in seq_len(length(Attr))) {
            h5writeAttribute(Attr[[i]], h5obj, name = names(Attr)[i])
        }
    }
    invisible(res)
}

#' @rdname h5_write
#' @export
h5writeDataset <- function(obj, h5loc, name, ...) {
    h5checktype(h5loc, "loc")
    res <- UseMethod("h5writeDataset")
    invisible(res)
}

#' @rdname h5_write
#' @export h5writeDataset.data.frame
h5writeDataset.data.frame <- function(obj, h5loc, name, level=6, chunk, DataFrameAsCompound = TRUE) {
    if (DataFrameAsCompound) {
        if (H5Lexists(h5loc, name)) {
            stop("Cannot write data.frame. Object already exists. Subsetting for compound datatype not supported.")
        }
        if (!is.null(level)) { 
            level = as.integer(level)
            if(missing(chunk)) 
                chunk <- nrow(obj)
        }
        did <- .Call("_h5createDataFrame", obj, h5loc@ID, name, level, as.integer(chunk), PACKAGE='rhdf5')
        .Call("_h5writeDataFrame", obj, did, PACKAGE='rhdf5')
        .Call("_H5Dclose", did, PACKAGE='rhdf5')
        res <- 0
    } else {
        a <- attr(obj,"names")
        if (is.null(a)) {
            attr(obj,"names") = sprintf("col%d",seq_len(ncol(obj)))
        } else {
            if (any(duplicated(a))) {
                a[duplicated(a)] = sprintf("col%d",seq_len(ncol(obj)))[duplicated(a)]
                attr(obj,"names") = a
            }
        }
        ## we can't write out factors, so convert any to character
        colClass <- sapply(obj, is.factor)
        if(any(colClass)) {
            obj[ which(colClass) ] <- lapply(obj[ ,which(colClass), drop=FALSE], FUN = as.character)
        }
        
        res <- h5writeDataset.list(obj=obj, h5loc=h5loc, name=name, level=level)
    }
    invisible(res)
}

#' @export h5writeDataset.list
h5writeDataset.list <- function(obj, h5loc, name, level=6) {
    exists <- try( { H5Lexists(h5loc, name) } )
    if (exists) {
        message("Existing object within HDF5 file cannot be overwritten with a list object. First delete the group or dataset from the HDF5 file.")
        res = 0
    } else {
        N = names(obj)
        newnames = FALSE
        if (is.null(N)) {
            newnames = TRUE
        } else {
            if (any(nchar(N) == 0)) {
                newnames = TRUE
            } else {
                if (length(N) != length(obj)) {
                    newnames = TRUE
                }
            }
        }
        if (newnames) {
            N = sprintf("ELT%d",seq_len(length(obj)))
        }
        res = NULL
        h5createGroup(h5loc, name)
        gid = H5Gopen(h5loc, name)
        for (i in seq_len(length(obj))) {
            res = h5write(obj[[i]], gid, N[i])
        }
        H5Gclose(gid)
    }
}

#' @export h5writeDataset.matrix
h5writeDataset.matrix    <- function(...) { h5writeDataset.array(...) }

#' @export h5writeDataset.integer
h5writeDataset.integer   <- function(...) { h5writeDataset.array(...) }

#' @export h5writeDataset.double
h5writeDataset.double    <- function(...) { h5writeDataset.array(...) }

#' @export h5writeDataset.logical
h5writeDataset.logical   <- function(...) { h5writeDataset.array(...) }

#' @export h5writeDataset.character
h5writeDataset.character <- function(...) { h5writeDataset.array(...) }

#' @export h5writeDataset.raw
h5writeDataset.raw       <- function(...) { h5writeDataset.array(...) }

#' @rdname h5_write
#' @export h5writeDataset.array
h5writeDataset.array <- function(obj, h5loc, name, index = NULL, 
                                 start=NULL, stride=NULL, block=NULL, count=NULL, 
                                 size=NULL, variableLengthString=FALSE, encoding=c("ASCII", "UTF-8"),
                                 level=6) {

    exists <- try( { H5Lexists(h5loc, name) } )
    if (!exists) {
        if (storage.mode(obj) == "character" && !variableLengthString && is.null(size)) {
            if (length(obj) > 0) {
                size <- max(nchar(obj), na.rm = TRUE)
                ## if any NA, the minimum string length is 2
                if(any(is.na(obj)) && size < 2) { size <- 2 }
                ## empty string gives size 0, and errors
                if(size == 0) { size <- 1 }
            } else {
                size <- 1
            }
        }
        if (is.null(dim(obj))) {
            dim <- length(obj) 
        } else {
            dim <- dim(obj)
            if (h5loc@native) dim <- rev(dim)
        }
        h5createDataset(h5loc, name, dim, storage.mode = storage.mode(obj), 
                        size = size, encoding = match.arg(encoding),
                        chunk=dim, level=level) 
    }
    h5dataset <- H5Dopen(h5loc, name)
    on.exit( H5Dclose(h5dataset) )
    h5writeDatasetHelper(obj=obj, h5dataset=h5dataset, index = index, start = start, stride = stride, 
                         block = block, count = count)
    h5writeAttribute(1L, h5dataset, name = "rhdf5-NA.OK")
    
    if(storage.mode(obj) == "character" && any(is.na(obj))) {
        h5writeAttribute(1L, h5dataset, name = "as.na")
        if(any(obj == "NA", na.rm = TRUE)) {
            warning("Both NA_character_ and the string 'NA' detected.\n",
                    "These will all be coerced to NA_character_ when read using h5read()")
        }
    }

    invisible(NULL)
}


