\name{as.hdf5.dataset}
\alias{as.hdf5.dataset}
\title{ Create an HDF5 dataset from an R array or vector. }
\description{
  An R array or vector will be transformed into an HDF5 object.
}
\usage{
as.hdf5.dataset(file, value, name)
}

\arguments{
  \item{file}{An object of type \code{hdf5.file} or \code{hdf5.group}
    that will contain the dataset.  }
  \item{value}{The R array or vector to be turned in to an HDF5 dataset. }
  \item{name}{ A string giving the name that the new dataset will have
    in \code{file}. }
}
\details{
  The R object is turned in to an HDF5 dataset of the corresponding
  type. It is stored in the file or group specified in the \code{file}
  argument. 
}
\value{
 An object of class \code{hdf5.dataset} is returned.
}
\author{ R. Gentleman }

\seealso{\code{\link{hdf5.dataset}} }

\examples{
\dontrun{
   mad <- hdf5.open("microarray.h5")
   x <- matrix(rnorm(100), nc=2)
   j <- as.hdf5.dataset(mad, x)
}}
\keyword{manip }

