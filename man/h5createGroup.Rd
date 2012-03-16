\name{h5createGroup}
\alias{h5createGroup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create HDF5 group}
\description{
Creates a group within an HDF5 file.
}
\usage{
h5createGroup (file, group)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{The filename (character) of the file in which the dataset will be located. For advanced programmers it is possible to provide an object of class \code{\link{H5IdComponent}} representing a H5 location identifier (file or group). See \code{\link{H5Fcreate}}, \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}} to create an object of this kind.}
  \item{group}{The name of the new group. The name can contain a hierarchy of groupnames, e.g. 'group1/group2/newgroup', but the function will fail if the top level group do not exists.}
}
\details{
Creates a new group within an HDF5 file. 
}
\value{Returns TRUE is group was created successfully and FALSE otherwise.}
\references{\url{http://www.hdfgroup.org/HDF5}}
\author{Bernd Fischer}
\seealso{
\code{\link{h5createFile}}, \code{\link{h5createDataset}}, \code{\link{h5read}}, \code{\link{h5write}}, \link{rhdf5}
}
\examples{
h5createFile("ex_createGroup.h5")

# create groups
h5createGroup("ex_createGroup.h5","foo")
h5createGroup("ex_createGroup.h5","foo/foobaa")

h5ls("ex_createGroup.h5")
}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
