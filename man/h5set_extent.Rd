\name{h5set_extent}
\alias{h5set_extent}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Set a new dataset extension}
\description{Set a new dataset extension to an existing dataset in an HDF5 file}
\usage{
h5set_extent(file, dataset, dims)
}
\arguments{
  \item{file}{The filename (character) of the file in which the dataset will be located. For advanced programmers it is possible to provide an object of class \code{\link{H5IdComponent}} representing a H5 location identifier (file or group). See \code{\link{H5Fcreate}}, \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}} to create an object of this kind.}
  \item{dataset}{The name of the dataset in the HDF5 file, or an object of class \code{\link{H5IdComponent}} representing a H5 dataset identifier. See \code{\link{H5Dcreate}}, or \code{\link{H5Dopen}} to create an object of this kind.}
  \item{dims}{The dimensions of the array as they will appear in the file. Note, the dimensions will appear in inverted order when viewing the file with a C-programm (e.g. HDFView), because the fastest changing dimension in R is the first one, whereas the fastest changing dimension in C is the last one.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Returns 0 if the dimension of the dataset was changed successfully and a negative value otherwise.}
\references{\url{http://www.hdfgroup.org/HDF5}}
\author{Bernd Fischer}
\seealso{\link{rhdf5}}
\examples{
library(rhdf5)
tmpfile <- tempfile()
h5createFile(file=tmpfile)
h5createDataset(tmpfile, "A", c(10,12), c(20,24))
h5ls(tmpfile, all=TRUE)[c("dim", "maxdim")]
h5set_extent(tmpfile, "A", c(20,24))
h5ls(tmpfile, all=TRUE)[c("dim", "maxdim")]
}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
