\name{is.finite}
\alias{is.finite}
\alias{is.finite.default}
\alias{is.finite.hdf5.dataset}
\title{ Test to see if elements are finite. }
\description{
  An array of logicals (with the same dimensions as \code{x}) is
  produced.
  Each element indicates whether the corresponding value of \code{x} is
  finite or not.
}
\usage{
is.finite(x, ...)
is.finite.hdf5.dataset(x, ...)
}
\arguments{
  \item{x}{Any object in the first case and an object of class
    \code{hdf5.dataset} in the second. }
  \item{\dots}{ Additional arguments. }
}
\details{
  For HDF5 datasets this can create a very large object. Currenly we
  create in the temporary HDF5 working directory. It would be nice to
  allow a location parameter to be specified as well.

  This makes \code{is.finite} generic.
}
\value{
  An object of the same dimensions (and for HDF5 it will also be an HDF5
  dataset) as \code{x} containing logical values indicating whether or
  not the corresponding elements of \code{x} are finite.
}
\author{R. Gentleman }

\seealso{\code{\link[hdf5]{as.double.hdf5.dataset}} }

\examples{
    mad <- hdf5.open("microarray.h5")
    x1 <- hdf5.dataset(mad, "finiteex", dim=c(15, 15))
    set.seed(098)
    x1[] <- rnorm(225)
    x1[3,4:8] <- Inf
    is.finite(x1)

}
\keyword{programming }
