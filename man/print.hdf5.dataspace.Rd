\name{print.hdf5.dataspace}
\alias{print.hdf5.dataspace}
\alias{print.hdf5.dataset}
\alias{print.hdf5.file}
\alias{print.hdf5.group}
\alias{print.hdf5.proplist}
\title{ Printing of the various HDF5 objects }
\description{
   In most cases the printing of an HDF5 object is done using a textual
   description. In part this is because the HDF5 objects are generally
   very large.
}
\usage{
print.hdf5.dataspace(x, ...)
print.hdf5.dataset(x, ...)
print.hdf5.file(x, ...)
print.hdf5.group(x, ...)
print.hdf5.proplist(x, ...)
}
\arguments{
  \item{x}{An HDF5 object of the specific type. }
  \item{\dots}{ Additional arguments, pretty much ignored in the current
  implementation.}
}
\details{
  These functions basically return a text string describing the object
  (actually they are using \code{Rprintf} and that should change).
}
\value{
  Currently they all return \code{NULL} and probably they should be
  changed to return the text string.
}
\author{B. Ellis and R. Gentleman }

\seealso{\code{\link{print}}}

\examples{

}
\keyword{ print }
