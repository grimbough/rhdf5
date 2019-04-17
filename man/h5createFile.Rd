\name{h5createFile}
\alias{h5createFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create HDF5 file}
\description{R function to create an empty HDF5 file.}
\usage{
h5createFile (file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{The filename of the HDF5 file.}
}
\details{
Creates an empty HDF5 file.
}
\value{Returns TRUE is file was created successfully and FALSE otherwise.}
\references{\url{https://portal.hdfgroup.org/display/HDF5}}
\author{Bernd Fischer}
\seealso{
\code{\link{h5createGroup}}, \code{\link{h5createDataset}}, \code{\link{h5read}}, \code{\link{h5write}}, \link{rhdf5}
}
\examples{
h5createFile("ex_createFile.h5")

# create groups
h5createGroup("ex_createFile.h5","foo")
h5createGroup("ex_createFile.h5","foo/foobaa")

h5ls("ex_createFile.h5")
}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
