
h5errorHandling <- function(type = "normal") {

  t = switch(type,
         "suppress" = 0L,
         "verbose" = 2L,
         1L)
  .Call("_h5errorHandling", t, PACKAGE = "rhdf5")

  invisible(NULL)
}

