\name{space.hdf5.dataset}
\alias{space}
\alias{space.hdf5.dataset}
\title{ Obtain a dataspace congruent with the supplied HDF5 dataset. }
\description{
  Selection of elements (and a few other things) for HDF5 datasets is
  carried out via a data space. These function provides a handle to a
  dataspace that can be used for various purposes.
}
\usage{
space(set, ...)
space.hdf5.dataset(set, ...)
}
\arguments{
  \item{set}{An HDF5 dataset. }
   \item{\dots}{Extra arguments, currently ignored.}
}
\details{
  This is an interface to the HDF5 function \code{H5Dget_space}.
}
\value{
  An object of class \code{hdf5.dataspace} is returned if successful.
}
\references{ \url{http://hdf.ncsa.uiuc.edu/} }
\author{R. Gentleman }

\seealso{ \code{\link[hdf5]{print.hdf5.dataspace}} }

\examples{
     x1 <- hdf5.open("aaa")
     x2 <- hdf5.dataset.create(x1, "hi", 10, 10)
     v  <- space(x2)
     v
}
\keyword{ manip }
