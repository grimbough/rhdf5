
h5loadData <- function(h5loc, L, all=FALSE, ..., native) {

    h5checktype(h5loc,"loc")
    if (length(L) > 0) {
        for (i in seq_len(length(L))) {
            if (is.data.frame(L[[i]])) {
                if (L[[i]]$ltype %in% h5constants[["H5L_TYPE"]][c("H5L_TYPE_HARD","H5L_TYPE_EXTERNAL")]) {
                    if (L[[i]]$otype == h5constants[["H5I_TYPE"]]["H5I_DATASET"]) {
                        L[i] = list(h5read(
                            h5loc, L[[i]]$name, ..., native = native
                        ))
                    } else {
                        L[i] = h5lsConvertToDataframe(
                            L[i], all=all, native=native
                        )
                    }
                } else {
                    L[i] = h5lsConvertToDataframe(L[i], all=all, native=native)
                }
            } else {
                group = H5Gopen(h5loc, names(L)[i])
                L[i] = list(
                    h5loadData(group, L[[i]], all=all, ..., native = native)
                )
                H5Gclose(group)
            }
        }
    }
    L
}

#' Dump the content of an HDF5 file.
#' 
#' @param file The filename (character) of the file in which the dataset will
#' be located. You can also provide an object of class [H5IdComponent-class] 
#' representing a H5 location identifier (file or group). See [H5Fcreate()], 
#' [H5Fopen()], [H5Gcreate()], [H5Gopen()] to create an object of this kind.
#' @param recursive If `TRUE`, the content of the whole group hierarchy is
#' listed. If `FALSE`, Only the content of the main group is shown. If a positive
#' integer is provided this indicates the maximum level of the hierarchy that
#' is shown.
#' @param all If `TRUE`, a longer list of information on each entry is provided.
#' @param index_type See `h5const("H5_INDEX")` for possible arguments.
#' @param order See `h5const("H5_ITER")` for possible arguments.
#' @param load If `TRUE` the datasets are read in, not only the header
#' information. Note, that this can cause memory problems for very large files.
#' In this case choose `load=FALSE` and load the datasets successively.
#' @param s3 Logical value indicating whether the file argument should be
#' treated as a URL to an Amazon S3 bucket, rather than a local file path.
#' @param s3credentials A list of length three, providing the credentials for
#' accessing files in a private Amazon S3 bucket.
#' @param native An object of class `logical`. If TRUE, array-like objects
#' are treated as stored in HDF5 row-major rather than R column-major
#' orientation. Using `native = TRUE` increases HDF5 file portability
#' between programming languages. A file written with `native = TRUE`
#' should also be read with `native = TRUE`
#' @param \dots Arguments passed to [h5read()]
#' 
#' @return Returns a hierarchical list structure representing the HDF5
#' group hierarchy. It either returns the datasets within the list structure
#' (`load=TRUE`) or it returns a `data.frame` for each dataset with the
#' dataset header information (`load=FALSE`).
#' 
#' @author Bernd Fischer, Mike L. Smith
#' @seealso [h5ls()]

#' @keywords programming interface IO file
#' @examples
#' 
#' h5File <- tempfile(pattern = "ex_ls_dump.h5")
#' h5createFile(h5File)
#' 
#' # create groups
#' h5createGroup(h5File,"foo")
#' h5createGroup(h5File,"foo/foobaa")
#' 
#' # write a matrix
#' B = array(seq(0.1,2.0,by=0.1),dim=c(5,2,2))
#' attr(B, "scale") <- "liter"
#' h5write(B, h5File,"foo/B")
#' 
#' # list content of hdf5 file
#' h5dump(h5File)
#' 
#' # list content of an hdf5 file in a public S3 bucket
#' \donttest{
#' h5dump(file = "https://rhdf5-public.s3.eu-central-1.amazonaws.com/h5ex_t_array.h5", s3 = TRUE)
#' }
#' 
#' @name h5_dump
#' @export
h5dump <- function( file, recursive = TRUE, load=TRUE, all=FALSE, 
                    index_type = h5default("H5_INDEX"), order = h5default("H5_ITER"), 
                    s3 = FALSE, s3credentials = NULL, ..., native=FALSE) {

    if(isTRUE(s3)) {
        fapl <- H5Pcreate("H5P_FILE_ACCESS")
        on.exit(H5Pclose(fapl))
        H5Pset_fapl_ros3(fapl, s3credentials)
        loc <- h5checktypeOrOpenLocS3(file, readonly = TRUE, fapl = fapl, native = native)
    } else {
        loc <- h5checktypeOrOpenLoc(file, readonly = TRUE, fapl = NULL, native = native)
    }
    on.exit(h5closeitLoc(loc), add = TRUE)
    
    index_type <- h5checkConstants( "H5_INDEX", index_type )
    order <- h5checkConstants( "H5_ITER", order )
    if (is.logical(recursive)) {
        if (recursive) {
            depth = -1L
        } else {
            depth = 1L
        }
    } else if (is.numeric(recursive) | is.integer(recursive) ) {
        depth = as.integer(recursive)
        if( length(recursive) > 1 ) {
            warning("'recursive' must be of length 1.  Only using first value.")
        } else if (recursive == 0) {
            stop("value 0 for 'recursive' is undefined, either a positive number specify the depth to descend or negative for maximum recursion")
        }
    } else {
        stop("'recursive' must be an integer of length 1 or a logical")
    }
    L <- .Call("_h5dump", loc$H5Identifier@ID, depth, index_type, order, PACKAGE='rhdf5')
    if (load) {
        L <- h5loadData( loc$H5Identifier, L, all=all, ..., native=native)
    } else {
        L <- h5lsConvertToDataframe(L, all=all, native=native)
    }
    
    L
}

