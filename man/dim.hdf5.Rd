\name{dim.hdf5}
\alias{dim.hdf5}
\title{ Obtain the dimensions of an HDF5 object. }
\description{
  For objects of classes \code{hdf5.dataset} and \code{hdf5.dataspace}
  the HDF5 dimension is obtained and returned. For all other HDF5
  objects \code{NULL} is returned.
}
\usage{
dim.hdf5(x)
}

\arguments{
  \item{x}{Any object of class \code{hdf5}. }
}
\details{
  This function obtains the dimension for classes \code{hdf5.dataset}
  and \code{hdf5.dataspace}. 
}
\value{
  A vector containing the dimension information.
}
\author{R. Gentleman}

\seealso{\code{\link{dim}}, \code{\link[hdf5]{hdf5}} }

\examples{
\dontrun{   mad <- hdf5.open("microarray.h5")
   hdf5.dataset(mad, "xxx", dim=c(100, 110))
   dim(mad$xxx)
}}
\keyword{manip }
