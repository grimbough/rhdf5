\name{Extract.hdf5}
\alias{[.hdf5.dataset}
\alias{[<-.hdf5.dataset}
\alias{$.hdf5.file}
\alias{$.hdf5.group}
\title{ Extract Parts of an HDF5 object. }
\description{
  These are subset operators for HDF5 objects. The name provides
  specialization (often via method dispatch) for the different specific
  types of HDF5 objects.
}
\usage{
  x[i]
  x[i, j, ...]
  x[i, j, ..., drop=TRUE]
  x$name
}
\arguments{
  \item{x}{An object of class \code{hdf5}. }
  \item{i}{An index into the object.}
  \item{j}{An index into the object.}
  \item{\dots}{ Optional arguments. }
  \item{drop}{A logical value, if \code{TRUE} the result
    is coerced to the lowest possible dimension }
  \item{name}{The element of a file or group to extract.}
}
\details{
  These operations only make sense for specific types of HDF5
  objects. The names of the functions indicate their specialization.
  There is currently no \code{[[} operator.

  For HDF5 groups and files the \code{$} operator extracts the
  appropriate element. 
}
\value{
  For the \code{$} operator an HDF5 object of the appropriate class is
  returned. For the \code{[} operator an R vector containing the subset
  is returned. It is envisaged that at some point we may make the
  \code{[} operator smarter and allow the return of an HDF5 object in
  situations where the returned value will be very large.
}
\author{B. Ellis and R. Gentleman }

\seealso{\code{\link[hdf5]{hdf5.dataset}}}

\examples{
  mad <- hdf5.open("microarray.h5")
  hdf5.group(mad,"chip")
  hdf5.group(mad$chip,"group")
  hdf5.group.rm(mad$chip,"group")
  x1 <- hdf5.dataset(mad$chip, "AA", dim=c(5, 5))
  x1[,1, drop=FALSE]

}
\keyword{ array }
\keyword{list}
