\name{hdf5.apply}
\alias{hdf5.apply}
\title{HDF5 Group Apply}
\description{
  Allows the user to iterate over a set of datasets within a group,
  optionally selecting a subregion from each dataset that is passed to
  the function specified by "fun."
}
\usage{
hdf5.apply(X, REGION, FUN,...)
}
\arguments{
  \item{X}{a group reference}
  \item{REGION}{a subset specification or set to NULL to pass the
    dataset itself}
  \item{FUN}{The function to call on each returned value. By default it
    is function(x) x}
  \item{...}{Additional arguments, currently unused}
}
\examples{
  mad <- hdf5.open("test.h5")
  hdf5.dataset(mad, "test", matrix(rnorm(1000), nc=20))
  hdf5.dataset(mad, "test2", rnorm(100))
  hdf5.apply(mad, list(1,), mean)
  hdf5.apply(mad, NULL, min)
  unlink("test.h5")
}
\seealso{\code{\link[hdf5]{hdf5.groups}}}
\keyword{manip}
