\name{&-methods}
\docType{methods}
\alias{&,H5IdComponent,ANY-method}
\title{Methods for Function \code{&} in Package \pkg{rhdf5}}
\description{
Methods for function \code{&} in package \pkg{rhdf5}.
}
\section{Methods}{
\describe{

\item{\code{signature(e1 = "H5IdComponent", e2 = "ANY")}}{
Returns a group handle or dataset handle for the group or dataset e2 in the HDF5 location e1. e1 can either be a file handle as returned by H5Fopen() or a group handle as e.g. returned by h5f$g1 or h5f$'/g1/g2's.
}
}}
\keyword{methods}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
