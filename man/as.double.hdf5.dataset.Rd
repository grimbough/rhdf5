\name{as.double.hdf5.dataset}
\alias{as.double.hdf5.dataset}
\alias{as.single.hdf5.dataset}
\title{ Coercion functions for HDF5 datasets. }
\description{
  These should coerce an object of class \code{hdf5.dataset} to have the
  specified type. Coercion means generally the creation of a new object.
}
\usage{
as.double.hdf5.dataset(x, ...)
as.single.hdf5.dataset(x, ...)
}
\arguments{
  \item{x}{An object of class \code{hdf5.dataset}. }
  \item{\dots}{Additional arguments. }
}
\details{
  These functions are currently dangerous as they do nothing.
  The problem with implementing them is that coercion will generally
  mean duplication. Since these are large HDF5 objects duplication
  probably means the creation of new HDF5 datasets. That raises the
  question of where these should be created.

  It would probably be useful to allow a location, or hdf5.file or
  hdf5.group argument to indicate where the copy should go.
}
\value{
  An object of the same dimensions as \code{x} but of the requested
  type. If \code{x} is already of the requested type then no action is
  taken. 
}
\author{ R. Gentleman }

\seealso{\code{\link{hdf5.dataset}} }

\examples{
## No examples until we resolve the copying issue.
}
\keyword{ manip }
