\name{hdf5.plist.create}
\alias{hdf5.plist.create}
\alias{hdf5.get.default.plist}
\title{ Create an HDF5 property list. }
\description{
  The HDF5 library provides a general means of interacting with files
  through a property list. We provide some simple tools for interacting
  with that property list.
}
\usage{
hdf5.plist.create(type, ...)
hdf5.get.default.plist()
}
\arguments{
  \item{type}{Is one of \code{file.create}, \code{file.access},
  \code{dataset},  \code{dataset.xfer}, \code{mount}}
  \item{\dots}{ Additional parameters depending upon the value of the
    first argument. }
}
\details{
  Arguments are passed through the HDF5 library routines for each of
  these. Property lists provide mechanisms for controlling file
  creation, file   access, dataset creation, dataset transfer and mounting.
}
\value{
  An object of class \code{hdf5.proplist}. This can be used to control the
  different interactions as described above and as described in section
  H5P of the HDF5 libray reference.
}
\references{\url{http://hdf.ncsa.uiuc.edu/} }
\author{B. Ellis and R. Gentleman }
\note{ This interface could use some work. }

\seealso{ \code{\link[hdf5]{hdf5.dataset}} }

\examples{
  hdf5.get.default.plist()
}
\keyword{ programming }
