#' HDF5 library constants.
#' 
#' Access to HDF5 constants.
#' 
#' These functions provide a list of HDF5 constants that are defined in the R
#' package. \code{h5constType} provides a list of group names and
#' \code{h5const} gives the constants defined within a group. \code{h5default}
#' gives the default choice for each group.
#' 
#' @param type A character name of a group of constants.
#' @return A character vector with names of HDF5 constants or groups.
#' @author Bernd Fischer
#' @examples
#' 
#' h5constType()[1]
#' h5const(h5constType()[1])
#' @name h5constants
NULL

#' @rdname h5constants
#' @export h5const
h5const <- function( type = "" ) {
  if (!exists("h5constants")) {
    h5constants <<- H5loadConstants()
  }
  names(h5constants[[type]])
}

#' @rdname h5constants
#' @export
h5constType <- function( ) {
  h5constants <- H5loadConstants()
  names(h5constants)
}

#' @rdname h5constants
#' @export
h5default <- function( type = "" ) {
  if (!exists("h5constants")) {
    h5constants <<- H5loadConstants()
  }
  names(h5constants[[type]])[1]
}

H5loadConstants <- function( ) {
  invisible(.Call("_H5constants", PACKAGE='rhdf5'))
}

h5constants = list()

h5checkConstants <- function(group, constant) {
  if (!exists("h5constants")) {
    h5constants <<- H5loadConstants()
  }
  res <- h5constants[[group]][constant[1]]
  if (is.null(res)) {
    stop("unknown 'group' of H5 constants")
  }
  if (length(constant) > 1) {
    warning("H5 constant identifier has more than one value. Only the first value will be used.")
  }
  res
}

h5const2Factor <- function( group = "", values ) {
  if (!exists("h5constants")) {
    h5constants <<- H5loadConstants()
  }
  if (!(group %in% names(h5constants))) {
    stop("unknown 'group' of H5 constants")
  }
  Levels <- unique(c(h5constants[[group]],values))
  f = factor(values,levels=Levels)
  m <- match(Levels, h5constants[[group]])
  LevelNames <- as.character(Levels)
  LevelNames[is.finite(m)] <- names(h5constants[[group]])[which(is.finite(m))]
  levels(f) = LevelNames
  f
}

h5const2String <- function( group = "", values ) {
  if (!exists("h5constants")) {
    h5constants <<- H5loadConstants()
  }
  if (!(group %in% names(h5constants))) {
    stop("unknown 'group' of H5 constants")
  }
  
  group_const <- h5constants[[group]]
  names(group_const)[match(values, group_const)]
}
