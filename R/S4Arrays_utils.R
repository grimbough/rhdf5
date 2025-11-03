# Copied from S4Arrays
extract_Nindex_from_syscall <- function(call, eframe) {
  Nindex <- lapply(seq_len(length(call) - 2L), function(i) {
    subscript <- call[[2L + i]]
    if (missing(subscript)) {
      return(NULL)
    }
    subscript <- eval(subscript, envir = eframe, enclos = eframe)
    if (is.null(subscript)) {
      return(integer(0))
    }
    subscript
  })
  argnames <- tail(names(call), n = -2L)
  if (!is.null(argnames)) {
    Nindex <- Nindex[!(argnames %in% c("drop", "exact", "value"))]
  }
  if (length(Nindex) == 1L && is.null(Nindex[[1L]])) {
    Nindex <- Nindex[0L]
  }
  Nindex
}
