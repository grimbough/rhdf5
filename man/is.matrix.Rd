\name{is.matrix}
\alias{is.matrix}
\alias{is.matrix.default}
\alias{is.matrix.hdf5.dataset}
\title{ A generic is.matrix with some methods.  }
\description{
  The function \code{is.matrix} returns true if the object \code{x} has
  dimension equal to two and both values are integers.
}
\usage{
is.matrix(x, ...)
is.matrix.hdf5.dataset(x, ...)
}
\arguments{
  \item{x}{ Any object for the first case and an object of class
    \code{hdf5.dataset} for the second. }
  \item{\dots}{Additional arguments, currently these are ignored. }
}
\details{
  As above.
}
\value{
  A logical, either \code{TRUE} if the conditions are met or
  \code{FALSE} if they are not.
}
\author{R. Gentleman }

\seealso{\code{\link[hdf5]{is.finite.hdf5.dataset}} }

\examples{
    mad <- hdf5.open("microarray.h5")
    x1 <- hdf5.dataset(mad, "finiteex", dim=c(15, 15))
    is.matrix(x1)
}
\keyword{ programming }
