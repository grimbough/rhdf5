\name{as.matrix.hdf5.dataset}
\alias{as.matrix.hdf5.dataset}
\title{ Coerce an HDF5 dataset to be an R matrix. }
\description{
   If an object of class \code{hdf5.dataset} has dimension two then it
   can be coerced into an \R matrix.  This function carries out the
   coercion. Be careful though as these can be very large.
}
\usage{
as.matrix.hdf5.dataset(x)
}
\arguments{
  \item{x}{An object of class \code{hdf5.dataset}. }
}
\details{
  The coercion may require a type coercion from the specific HDF5 type
  to one of the standard \R types (such as integer or double).
}
\value{
  A duplicate of \code{x} as an \R object if \code{x} has dimension two.
}

\author{ R. Gentleman }


\seealso{\code{\link[hdf5]{is.matrix.hdf5.dataset}} }

\examples{
    mad <- hdf5.open("microarray.h5")
    x1 <- hdf5.dataset(mad, "asmat", dim=c(31, 12))
    set.seed(777)
    x1[] <- rexp(372,4)
    y <- as.matrix(x1)
}
\keyword{ programming }
