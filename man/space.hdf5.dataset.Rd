\name{space.hdf5.dataset}
\alias{space}
\alias{space.hdf5.dataset}
\alias{hdf5.select.hyperslab}
\title{ Obtain a dataspace congruent with the supplied HDF5 dataset. }
\description{
  Selection of elements (and a few other things) for HDF5 datasets is
  carried out via a data space. These function provides a handle to a
  dataspace that can be used for various purposes.
}
\usage{
space(set, ...)
space.hdf5.dataset(set, ...)
hdf5.select.hyperslab(space, selop = "SET", start, stride, count, block)
}
\arguments{
  \item{set}{An HDF5 dataset. }
  \item{\dots}{Extra arguments, currently ignored.}
  \item{space}{An instance of the \code{hdf5.dataspace} class.}
  \item{selop}{The selection operator. One of \code{SET} or \code{OR}.}
  \item{start}{The start location for each hyperslab.}
  \item{stride}{The stride for each hyperslab.}
  \item{count}{The count for each hyperslab.}
  \item{block}{The block for each hyperslab.}
}
\details{
  \code{space.hdf5.dataset} is an interface to the HDF5 function
  \code{H5Dget_space}.

  \code{hdf5.select.hyperslab} is an interface to the HDF5 function
  \code{H5Sselect_hyperslab}.

  Readers are referred to the HDF5 documentation for specific details.
}
\value{
  \code{space.hdf5.dataset} returns an object of class
  \code{hdf5.dataspace} if successful.

  \code{hdf5.select.hyperslab} modifies the values in the \code{space}
  argument. 
}
\references{ \url{http://hdf.ncsa.uiuc.edu/} }
\author{R. Gentleman }

\seealso{ \code{\link[hdf5]{print.hdf5.dataspace}} }

\examples{
     x1 <- hdf5.open("aaa")
     x2 <- hdf5.dataset(x1, "hi", dims=c(10, 10))
     v  <- space(x2)
     v
}
\keyword{ manip }
