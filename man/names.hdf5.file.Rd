\name{names.hdf5.file}
\alias{names.hdf5.file}
\alias{names.hdf5.group}
\title{ Obtain the names for datasets and or groups.  }
\description{
  These functions provide a listing of the names of datasets and or
  groups within HDF5 files and groups.
}
\usage{
names.hdf5.file(x, \dots)
names.hdf5.group(x, \dots)
}
\arguments{
  \item{x}{An object of class \code{hdf5.group} or \code{hdf5.file}. }
  \item{\dots}{Other arguments, this is not currently used.}
}
\details{
  The names of the HDF5 objects in the supplied file or group are
  returned in a vector. It might be nice to have a recursive version at
  some point.

  Unlike lists and some other \R objects these names are not an
  attribute of \code{x}.
}
\value{
  A character vector (or NULL) containing the names of the objects in
  \code{x}.
}

\author{B. Ellis and R. Gentleman }

\seealso{\code{\link{names}}, \code{\link[hdf5]{hdf5.file}} }

\examples{
  mad <- hdf5.open("microarray.h5")
  hdf5.group.create(mad,"chip")
  names(mad)
  hdf5.group.create(mad$chip, "bar")
  names(mad$chip)
}
\keyword{ attribute }
