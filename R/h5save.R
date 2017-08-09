
h5save <- function(..., file, name = NULL, createnewfile=TRUE) {
  N <- length(list(...))

  # get object names
  if (is.null(name)) {
    name <- as.character(substitute(list(...)))[-1L]
  }
  if (length(name) != N) {
    stop("length of 'name' argument  must fit the number of objects to save.")
  }

  loc = h5checktypeOrOpenLoc(file, createnewfile=createnewfile)

  for (i in 1:N) {
    h5write(list(...)[[i]], file = loc$H5Identifier, name = name[i])
  }

  h5closeitLoc(loc)
  invisible(NULL)
}

