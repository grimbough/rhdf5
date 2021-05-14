h5readDataset <- function (h5dataset, index = NULL, start = NULL, stride = NULL, 
                           block = NULL, count = NULL, compoundAsDataFrame = TRUE, drop = FALSE, ...) {

    h5spaceFile <- H5Dget_space(h5dataset)
    on.exit(H5Sclose(h5spaceFile))
    h5spaceMem = NULL
    if (!is.null(index)) {
        s <- H5Sget_simple_extent_dims(h5spaceFile)$size
        if (length(index) != length(s)) {
            stop("length of index has to be equal to dimensional extension of HDF5 dataset.")
        }
        
        index_null <- sapply(index, is.null)
        
        for (i in seq_along(index)) {
            if ( is.name(index[[i]]) | is.call(index[[i]]) ) {
                index[[i]] <- eval(index[[i]])  
            }
        }
        size <- .H5Sselect_dim( h5spaceFile, index)
        #size <- .H5Sselect_index( h5spaceFile, index, index_null)
        h5spaceMem <- H5Screate_simple(size, native = h5dataset@native)
        on.exit(H5Sclose(h5spaceMem), add = TRUE)
    }
    else {
        if (any(c(!is.null(start), !is.null(stride), 
                  !is.null(count), !is.null(block)))) {
            size = 0
            try({
                size = H5Sselect_hyperslab(h5spaceFile, 
                                           start = start, stride = stride, count = count, 
                                           block = block)
            })
            h5spaceMem = H5Screate_simple(size, native = h5dataset@native)
            on.exit(H5Sclose(h5spaceMem), add = TRUE)
        }
    }
    obj <- NULL
    tryCatch({
        obj <- H5Dread(h5dataset = h5dataset, h5spaceFile = h5spaceFile, 
                       h5spaceMem = h5spaceMem,
                       compoundAsDataFrame = compoundAsDataFrame, drop = drop, ...)
    },
        error = function(e) { 
            err <- h5checkFilters(h5dataset)
            ## if we fail here it doesn't make it to the usual H5Dclose call
            on.exit(H5Dclose(h5dataset))
            if(nchar(err) > 0)
                stop(err, call. = FALSE)
            else 
                stop(e)
        }
    )

    ## Here we reorder data to match the order requested in index.
    ## The calls to H5Sselect_index will have returned data linearly
    ## from the file, not the potentially random order requested.
    if (!is.null(index)) {
        I = list()
        for (i in seq_along(index)) {
            if(!index_null[i]) { ## skip if the index was generated inside this function
              tmp <- unique(index[[i]])
              if(is.unsorted(tmp)) { 
                tmp <- sort.int(tmp) 
              } 
              I[[i]] = match(index[[i]], tmp)
            } else {
              I[[i]] <- seq_len(s[i])
            }
        }
        obj.dim <- lapply(dim(obj), FUN = seq_len)
        ## only need to compare the dimensions not set automatically
        if(!identical(I[!index_null], obj.dim[!index_null])) {
            obj <- do.call("[", c(list(obj), I, drop = FALSE))
        } 
    }

    return(obj)
}

#' Reads and write object in HDF5 files
#' 
#' Reads objects in HDF5 files. This function can be used to read
#' either full arrays/vectors or subarrays (hyperslabs) from an
#' existing dataset.
#' 
#' Read an R object from an HDF5 file. If none of the arguments
#' \code{start, stride, block, count} are specified, the dataset has the same
#' dimension in the HDF5 file and in memory. If the dataset already exists in
#' the HDF5 file, one can read subarrays, so called hyperslabs from
#' the HDF5 file. The arguments \code{start, stride, block, count} define the
#' subset of the dataset in the HDF5 file that is to be read/written. See these
#' introductions to hyperslabs:
#' \url{https://support.hdfgroup.org/HDF5/Tutor/selectsimple.html},
#' \url{https://support.hdfgroup.org/HDF5/Tutor/select.html} and
#' \url{http://ftp.hdfgroup.org/HDF5/Tutor/phypecont.html}. Please note that in
#' R the first dimension is the fastest changing dimension.
#' 
#' When viewing the HDF5 datasets with any C-program (e.g. HDFView), the order
#' of dimensions is inverted. In the R interface counting starts with 1,
#' whereas in the C-programs (e.g. HDFView) counting starts with 0.
#' 
#' @param file The filename (character) of the file in which the dataset is
#' be located. It is possible to provide an object of
#' class \code{\link{H5IdComponent}} representing a H5 location identifier
#' (file or group). See \code{\link{H5Fcreate}}, \code{\link{H5Fopen}},
#' \code{\link{H5Gcreate}}, \code{\link{H5Gopen}} to create an object of this
#' kind.
#' @param h5loc An object of class \code{\link{H5IdComponent}} representing a
#' H5 location identifier (file or group). See \code{\link{H5Fcreate}},
#' \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}} to
#' create an object of this kind.
#' @param h5obj An object of class \code{\link{H5IdComponent}} representing a
#' H5 object identifier (file, group, or dataset). See \code{\link{H5Fcreate}},
#' \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}},
#' \code{\link{H5Dcreate}}, or \code{\link{H5Dopen}} to create an object of
#' this kind.
#' @param name The name of the dataset in the HDF5 file.
#' @param index List of indices for subsetting. The length of the list has to
#' agree with the dimensional extension of the HDF5 array. Each list element is
#' an integer vector of indices. A list element equal to NULL choses all
#' indices in this dimension. Counting is R-style 1-based.
#' @param start The start coordinate of a hyperslab (similar to subsetting in
#' R). Counting is R-style 1-based. This argument is ignored, if index is not
#' NULL.
#' @param stride The stride of the hypercube. Read the introduction
#' \url{http://ftp.hdfgroup.org/HDF5/Tutor/phypecont.html} before using this
#' argument. R behaves like Fortran in this example. This argument is ignored,
#' if index is not NULL.
#' @param block The block size of the hyperslab. Read the introduction
#' \url{http://ftp.hdfgroup.org/HDF5/Tutor/phypecont.html} before using this
#' argument. R behaves like Fortran in this example. This argument is ignored,
#' if index is not NULL.
#' @param count The number of blocks to be read. This argument is ignored,
#' if index is not NULL.
#' @param chunk Specifies the number of items to be include in an HDF5 chunk.
#' When writing a \code{data.frame} this represents the number of rows to be
#' included in a chunk.  If left unspecified the defaults is the smaller of:
#' the total number of rows or the number of rows that fit within 4GB of
#' memory.
#' @param native An object of class \code{logical}. If TRUE, array-like objects
#' are treated as stored in HDF5 row-major rather than R column-major
#' orientation. Using \code{native = TRUE} increases HDF5 file portability
#' between programming languages. A file written with \code{native = TRUE}
#' should also be read with \code{native = TRUE}
#' @param compoundAsDataFrame If true, a compound datatype will be coerced to a
#' data.frame. This is not possible, if the dataset is multi-dimensional.
#' Otherwise the compound datatype will be returned as a list. Nested compound
#' data types will be returned as a nested list.
#' @param callGeneric If TRUE a generic function h5read.classname will be
#' called if it exists depending on the dataset's class attribute within the
#' HDF5 file. This function can be used to convert the standard output of
#' h5read depending on the class attribute. Note that h5read is not a S3
#' generic function. Dispatching is done based on the HDF5 attribute after the
#' standard h5read function.
#' @param read.attributes (logical) If `TRUE`, the HDF5 attributes are read and
#' attached to the respective R object.
#' @param drop (logical) If TRUE, the HDF5 object is read as a vector with `NULL`
#' dim attributes.
#' @param s3 Logical value indicating whether the file argument should be
#' treated as a URL to an Amazon S3 bucket, rather than a local file path.
#' @param s3credentials A list of length three, providing the credentials for
#' accessing files in a private Amazon S3 bucket.
#' @param \dots Further arguments passed to \code{\link{H5Dread}}.
#' 
#' @return \code{h5read} returns an array with the data read.
#' 
#' @author Bernd Fischer, Mike Smith
#' @seealso \code{\link{h5ls}}
#' @examples
#' 
#' h5createFile("ex_hdf5file.h5")
#' 
#' # write a matrix
#' B = array(seq(0.1,2.0,by=0.1),dim=c(5,2,2))
#' h5write(B, "ex_hdf5file.h5","B")
#' 
#' # read a matrix
#' E = h5read("ex_hdf5file.h5","B")
#' 
#' # write and read submatrix
#' h5createDataset("ex_hdf5file.h5", "S", c(5,8), storage.mode = "integer", chunk=c(5,1), level=7)
#' h5write(matrix(1:5,nr=5,nc=1), file="ex_hdf5file.h5", name="S", index=list(NULL,1))
#' h5read("ex_hdf5file.h5", "S")
#' h5read("ex_hdf5file.h5", "S", index=list(NULL,2:3))
#' 
#' # Read a subset of an hdf5 file in a public S3 bucket
#' \donttest{
#' h5read('https://rhdf5-public.s3.eu-central-1.amazonaws.com/rhdf5ex_t_float_3d.h5', 
#'       s3 = TRUE, name = "a1", index = list(NULL, 3, NULL))
#' }
#' 
#' @name h5_read
#' @export h5read
h5read <- function(file, name, index=NULL, start=NULL, stride=NULL, block=NULL,
                   count = NULL, compoundAsDataFrame = TRUE, callGeneric = TRUE,
                   read.attributes=FALSE, drop = FALSE, ..., native = FALSE,
                   s3 = FALSE, s3credentials = NULL) {
    
    if(isTRUE(s3)) {
        fapl <- H5Pcreate("H5P_FILE_ACCESS")
        on.exit(H5Pclose(fapl))
        H5Pset_fapl_ros3(fapl, s3credentials)
        loc <- h5checktypeOrOpenLocS3(file, readonly = TRUE, fapl = fapl, native = native)
    } else {
        loc <- h5checktypeOrOpenLoc(file, readonly = TRUE, fapl = NULL, native = native)
    }
    on.exit(h5closeitLoc(loc), add = TRUE)
    
    if (!H5Lexists(loc$H5Identifier, name)) {
        stop("Object '", name, "' does not exist in this HDF5 file.")
    } else {
        oid = H5Oopen(loc$H5Identifier, name)
        on.exit(H5Oclose(oid), add = TRUE)
        type = H5Iget_type(oid)
        num_attrs = H5Oget_num_attrs(oid)
        if (is.na(num_attrs)) { num_attrs = 0 }
        if (type == "H5I_GROUP") {
            gid <- H5Gopen(loc$H5Identifier, name)
            obj = h5dump(gid, start=start, stride=stride, block=block, 
                         count=count, compoundAsDataFrame = compoundAsDataFrame, callGeneric = callGeneric, ...)
            H5Gclose(gid)
        } else if (type == "H5I_DATASET") {
            h5dataset <- H5Dopen(loc$H5Identifier, name)
            on.exit(H5Dclose(h5dataset), add = TRUE)
            obj <- h5readDataset(h5dataset, index = index, start = start, stride = stride, 
                                 block = block, count = count, compoundAsDataFrame = compoundAsDataFrame, drop = drop, ...)
            ## coerce the string "NA" to NA if required
            if(storage.mode(obj) == "character" && H5Aexists(h5dataset, name = "as.na")) {
                if(any(obj == "NA")) {
                    obj[obj == "NA"] <- NA_character_
                }
            }

            cl <- attr(obj,"class")
            if (!is.null(cl) & callGeneric) {
                if (exists(paste("h5read",cl,sep="."),mode="function")) {
                    obj <- do.call(paste("h5read",cl,sep="."), args=list(obj = obj))
                }
            }
        } else {
            message("Reading of object type not supported.")
            obj <- NULL
        } ## GROUP
        if (read.attributes & (num_attrs > 0) & !is.null(obj)) {
            for (i in seq_len(num_attrs)) {
                A = H5Aopen_by_idx(loc$H5Identifier, n = i-1, objname = name)
                attrname <- H5Aget_name(A)
                if (attrname != "dim") {
                    attr(obj, attrname) = H5Aread(A)
                }
                ## Don't put this in on.exit() 
                ## A is overwritten in the loop and we lose track of it
                H5Aclose(A)
            }
        }
    }  # !H5Lexists
    
    obj
}
