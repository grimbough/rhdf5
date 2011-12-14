
H5loadConstants <- function( ) {
  invisible(.Call("_H5constants", PACKAGE='rhdf5'))
}

h5constants = list()

h5default <- function( type = "" ) {
  if (!exists("h5constants")) {
    h5constants <<- H5loadConstants()
  }
  names(h5constants[[type]])[1]
}

h5const <- function( type = "" ) {
  if (!exists("h5constants")) {
    h5constants <<- H5loadConstants()
  }
  names(h5constants[[type]])
}

h5constType <- function( ) {
  ## if (!exists("h5constants")) {
    h5constants <- H5loadConstants()
  ## }
  names(h5constants)
}

h5checkConstants <- function(group, constant) {
  if (!exists("h5constants")) {
    h5constants <<- H5loadConstants()
  }
  res <- h5constants[[group]][constant[1]]
  if (is.null(res)) {
    stop("unknown 'group' of H5 constants")
  }
  if (is.null(res)) {
    if (constant %in% h5constants[[group]]) {
      res <- constant
    } else {
      stop("Constant ", constant, " unknown. Has to be one of '", paste(names(h5constants[[group]]), collapse="', '"), "'")
    }
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
  LevelNames[is.finite(m)] <- names(h5constants[[group]])[is.finite(m)]
  levels(f) = LevelNames
  f
}
