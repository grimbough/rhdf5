\name{[<--methods}
\docType{methods}
\alias{[<-,H5IdComponent-method}
\title{Methods for Function \code{[<-} in Package \pkg{rhdf5}}
\description{
Methods for function \code{[<-} in package \pkg{rhdf5}.
}
\section{Methods}{
\describe{

\item{\code{signature(x = "H5IdComponent")}}{
 Subsetting of an HDF5 dataset. The function writes an R data object to a subset of an HDF5 dataset. The given dimensions have to fit the dimensions of the HDF5 dataset. The HDF5 dataset has to be created beforehand, e.g. by h5createDataset().
}
}}
\keyword{methods}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
