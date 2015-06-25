\name{h5errorHandling}
\alias{h5errorHandling}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{handling of HDF5 error messages}
\description{Sets the options for handling HDF5 error messages.}
\usage{
h5errorHandling (type="normal")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{'normal' (default) shows a one line error message in R. 'verbose' shows the whole HDF5 error message. 'suppress' suppresses the HDF5 error messages completely.}
}
\details{
Sets the options for HDF5 error hanlding.
}
\value{Returns 0 if options are set successfully.}
\references{\url{http://www.hdfgroup.org/HDF5}}
\author{Bernd Fischer}
\seealso{
\link{rhdf5}
}
\examples{
h5errorHandling("normal")
}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
