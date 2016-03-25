\name{$<--methods}
\docType{methods}
\alias{$<-,H5IdComponent-method}
\title{ ~~ Methods for Function \code{$<-} in Package \pkg{rhdf5} ~~}
\description{
Methods for function \code{$<-} in package \pkg{rhdf5}.
}
\section{Methods}{
\describe{

\item{\code{signature(x = "H5IdComponent")}}{
Writes the assigned object to to the HDF5 file at location e1. e1 can either be a file handle as returned by H5Fopen() or a group handle as e.g. returned by h5f$g1 or h5f$'/g1/g2's. The storage.mode of the assigned object has to be compatible to the datatype of the HDF5 dataset. The dimension of the assigned object have to be identical the dimensions of the HDF5 dataset. To create a new HDF5 dataset with specific properties (e.g. compression level or chunk size), please use the function h5createDataset first.
}

}}
\keyword{methods}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
