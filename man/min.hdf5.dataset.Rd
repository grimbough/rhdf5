\name{min.hdf5.dataset}
\alias{min.hdf5.dataset}
\alias{max.hdf5.dataset}
\alias{range.hdf5.dataset}
\alias{length.hdf5.dataset}
\title{ Compute various useful quantities on HDF5 datasets. }
\description{
  These methods provide some basic numerical functionality that is
  needed to treat HDF5 datasets as if they were R arrays.
}
\usage{
min.hdf5.dataset(..., na.rm=FALSE)
max.hdf5.dataset(..., na.rm=FALSE)
range.hdf5.dataset(..., na.rm=FALSE)
length.hdf5.dataset(x)
}
\arguments{
  \item{...}{An object of class \code{hdf5.dataset}. }
  \item{na.rm}{A logical variable indicating whether NA's (missing
    values) should be removed when computing the summary statistic. }
}
\details{
  Not much to say. It might be nice to be able to handle multiple
  arguments (and then multiple classes of arguments) for \code{min},
  \code{max} and \code{range}.
}
\value{
 The appropriate summary statistic(s) in a vector.
}

\author{R. Gentleman }
\seealso{\code{\link{hdf5.dataset}} }

\examples{
 \dontrun{ set.seed(321)   
  xx <- hdf5.dataset(mad, "aab", dim=c(10, 10))
  xx[]<-rnorm(100)
  xx ##10x10 matrix of reals
  min(xx)  ## [1] -1.932862
  max(xx)  ## [1] 2.711432
  range(xx) ##[1] -1.932862  2.711432
  xx[5,5] <- NA
  min(xx) ##[1] NA
  min(xx,na.rm=TRUE)  ##[1] -1.932862
}}

\keyword{ math }
