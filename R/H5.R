#' HDF5 General Library Functions
#' 
#' @description These low level functions provide general library functions for HDF5.
#' 
#' @return 
#' * `H5open` initializes the HDF5 library. 
#' * `H5close` flushes all data to disk, closes all open identifiers, and cleans up memory.
#' * `H5garbage_collect` cleans up memory. 
#' * `H5get_libversion` returns the version number of the HDF5 C-library.
#' 
#' @author Bernd Fischer, Mike Smith
#' @examples
#' \dontrun{
#'   H5open()
#'   H5close()
#'   H5garbage_collect()
#'   H5get_libversion()
#' }
#' @name H5functions
NULL

#' @rdname H5functions
#' @export
H5open <- function( ) {
  invisible(.Call("_H5open", PACKAGE='rhdf5'))
}

#' @rdname H5functions
#' @export
H5close <- function( ) {
  invisible(.Call("_H5close", PACKAGE='rhdf5'))
}

#' @rdname H5functions
#' @export
H5garbage_collect <- function( ) {
  invisible(.Call("_H5garbage_collect", PACKAGE='rhdf5'))
}

#' @rdname H5functions
#' @export
H5get_libversion <- function( ) {
  .Call("_H5get_libversion", PACKAGE='rhdf5')
}

#' Close open HDF5 handles
#'
#' This functions can be used in two ways.  Firstly, it can be passed one or
#' more [H5IdComponent-class] objects and it'll will try to close all of them
#' regardless of the whether they represent a file, group, dataset etc.  This
#' can be easier than making multiple calls to [H5Fclose()], [H5Gclose()], etc.
#'
#' Secondly, cccasionally references to HDF5 files, groups, datasets etc can be
#' created and not closed correctly.  Maybe because a function stopped before
#' getting to the close statement, or the open handle was not assigned to an R
#' variable.  If no arguments are provide this function identifies all open
#' handles and closes them.
#'
#' @param ... One or more objects of class [H5IdComponent-class] which should be
#'   closed.  If nothing is provided to the function, all open handles will be
#'   closed.
#'
#' @return Doesn't return anything. Called for the side-effect of closing open
#'   HDF5 handles.
#'
#' @author Mike Smith
#' @keywords IO file
#' @examples
#'
#'
#' ## create an empty file and then re-open it
#' h5createFile("ex_h5closeAll.h5")
#' H5Fopen("ex_h5closeAll.h5")
#'
#' ## list all open identifiers
#' h5listIdentifier()
#'
#' ## close all open identifiers and verify
#' h5closeAll()
#' h5listIdentifier()
#'
#' @export h5closeAll
h5closeAll <- function(...) {
    
    objects <- list(...)
    if(length(objects) == 0) {
      objects <- h5validObjects()
    }
    invisible(lapply(objects, .H5close))
}

.H5close <- function(h5id){
    
    isvalid <- H5Iis_valid(h5id)
    if (!isvalid) {
        stop("Error in h5closeAll(). H5Identifier not valid.", call. = FALSE)
    }
    
    truetype <- as.character(H5Iget_type(h5id))
    
    closeFunc <- switch(truetype,
                        H5I_FILE = H5Fclose,
                        H5I_GROUP = H5Gclose,
                        H5I_DATASET = H5Dclose,
                        H5I_GENPROP_LST = H5Pclose,
                        H5I_DATASPACE = H5Sclose,
                        H5I_ATTR = H5Aclose,
                        stop("Error in h5closeAll(). Appropriate close function not found", call. = FALSE)
    )
    
    closeFunc(h5id)
    
}
