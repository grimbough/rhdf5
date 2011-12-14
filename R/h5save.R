
h5save <- function(..., file, name = NULL, createnewfile=TRUE) {
  N <- length(list(...))
  ## cat(N, " objects.\n")

  # get object names
  if (is.null(name)) {
    names <- as.character(substitute(list(...)))[-1L]
  }
  if (length(names) != N) {
    stop("length of names must fit the number of objects to save.")
  }
  ## cat(names, "\n")

  # open file if no loc_id provided
  if (is( file, "H5file" ) | is( file, "H5group" )) {
    h5loc = file
  } else {
    if (file.exists(file)) {
      ## print("Open file.")
      h5loc <- H5Fopen(file)
    } else {
      if (createnewfile) {
        ## print("Create new file.")
        h5loc <- H5Fcreate(file)
      } else {
        stop("File exists. Cannot write sae data.")
      }
    }
  }

  for (i in 1:N) {
    ## cat("H5save: Write object ",names[i],".\n")
    ## print(list(...)[[i]])
    h5write(list(...)[[i]], file = h5loc, name = names[i])
  }

  if (!is( file, "H5file" ) & !is( file, "H5group" )) {
    H5Fclose(h5loc)
  }
  invisible(NULL)
}

