\name{hdf5.apply}
\alias{hdf5.apply}
\title{HDF5 Group Apply}
\description{
  Allows the user to iterate over a set of datasets within a group,
  optionally selecting a subregion from each dataset that is passed to
  the function specified by "fun."
}
\usage{
hdf5.apply(group,REGION,FUN,...)
}
\arguments{
  \item{group}{a group reference}
  \item{REGION}{a subset specification or set to NULL to pass the
    dataset itself}
  \item{FUN}{The function to call on each returned value. By default it
    is function(x) x}
}
\examples{
mad <- hdf5.open("test.h5")
unlist(hdf5.apply(mad[["test"]],list(1,),mean))
}
\seealso{\code{\link[hdf5]{hdf5.groups}}}
\keyword{manip}
