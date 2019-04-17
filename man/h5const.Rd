\name{h5const}
\alias{h5const}
\alias{h5constType}
\alias{h5default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{HDF5 library constants.}
\description{
Access to HDF5 constants.
}
\usage{
h5const     (type = "")
h5default   (type = "")
h5constType ()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{A character name of a group of constants.}
}
\details{
These functions provide a list of HDF5 constants that are defined in the R package. \code{h5constType} provides a list of group names and  \code{h5const} gives the constants defined within a group. \code{h5default} gives the default choice for each group.
}
\value{
A character vector with names of HDF5 constants or groups.
}
\references{\url{https://portal.hdfgroup.org/display/HDF5}}
\author{Bernd Fischer}
\seealso{
\link{rhdf5}
}
\examples{
h5constType()[1]
h5const(h5constType()[1])
}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
