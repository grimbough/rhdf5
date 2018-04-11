\name{h5save}
\alias{h5save}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Saves a series of objects to an HDF5 file.}
\description{Saves a number of R objects to an HDF5 file.}
\usage{
h5save(..., file, name = NULL, createnewfile = TRUE, native = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{The objects to be saved.}
  \item{file}{The filename (character) of the file in which the dataset will be located. For advanced programmers it is possible to provide an object of class \code{\link{H5IdComponent}} representing a H5 location identifier (file or group). See \code{\link{H5Fcreate}}, \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}} to create an object of this kind.}
  \item{name}{A character vector of names for the datasets. The length of the name vector should match the number of objects.}
  \item{createnewfile}{If TRUE, a new file will be created if necessary.}
  \item{native}{An object of class \code{logical}. If TRUE, array-like objects are treated as stored in HDF5 row-major rather than R column-major orientation. Using \code{native = TRUE} increases HDF5 file portability between programming languages. A file written with \code{native = TRUE} should also be read with \code{native = TRUE}}.
}
\details{
The objects will be saved to the HDF5 file. If the file does not exists it will be created. The data can be read again by either \code{\link{h5dump}} or individually for each dataset by \code{\link{h5read}}.
}
\value{Nothing returned.}
\references{\url{http://www.hdfgroup.org/HDF5}}
\author{Bernd Fischer}
\seealso{
\code{\link{h5ls}}, \code{\link{h5write}}, \link{rhdf5}
}
\examples{
A = 1:7;  B = 1:18; D = seq(0,1,by=0.1)
h5save(A, B, D, file="ex_save.h5")
h5dump("ex_save.h5")
}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
