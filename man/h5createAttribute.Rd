\name{h5createAttribute}
\alias{h5createAttribute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create HDF5 attribute}
\description{R function to create an HDF5 attribute and defining its dimensionality.
}
\usage{
h5createAttribute (obj, attr, dims, maxdims = dims, file, 
                   storage.mode = "double", H5type = NULL, size=NULL,
                   cset = c("ASCII", "UTF8"), native = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{The name (character) of the object the attribute will be attatched to. For advanced programmers it is possible to provide an object of class \code{\link{H5IdComponent}} representing a H5 object identifier (file, group, dataset). See \code{\link{H5Fcreate}}, \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}}, \code{\link{H5Dcreate}}, \code{\link{H5Dopen}} to create an object of this kind.}
  \item{file}{The filename (character) of the file in which the dataset will be located. For advanced programmers it is possible to provide an object of class \code{\link{H5IdComponent}} representig an H5 location identifier. See \code{\link{H5Fcreate}}, \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}} to create an object of this kind. The \code{file} argument is not required, if the argument \code{obj} is of type \code{H5IdComponent}.}
  \item{attr}{Name of the attribute to be created.}
  \item{dims}{The dimensions of the attribute as a numeric vector. If \code{NULL}, a scalar dataspace will be created instead.}
  \item{maxdims}{The maximum extension of the attribute.}
  \item{storage.mode}{The storage mode of the data to be written. Can be obtained by \code{storage.mode(mydata)}.}
  \item{H5type}{Advanced programmers can specify the datatype of the dataset within the file. See \code{h5const("H5T")} for a list of available datatypes. If \code{H5type} is specified the argument \code{storage.mode} is ignored. It is recommended to use \code{storage.mode}}
  \item{size}{The maximum string length when \code{storage.mode='character'}. If this is specified, HDF5 stores each string of \code{attr} as fixed length character arrays. Together with compression, this should be efficient. 
  
  If this argument is set to \code{NULL}, HDF5 will instead store variable-length strings.
  }
  \item{cset}{The encoding to use when \code{storage.mode='character'}.}
  \item{native}{An object of class \code{logical}. If TRUE, array-like objects are treated as stored in HDF5 row-major rather than R column-major orientation. Using \code{native = TRUE} increases HDF5 file portability between programming languages. A file written with \code{native = TRUE} should also be read with \code{native = TRUE}}.
}
\details{
Creates a new attribute and attaches it to an existing HDF5 object. The function will fail, if the file doesn't exist or if there exists already another attribute with the same name for this object. 

You can use \code{\link{h5writeAttribute}} immediately. It will create the attribute for you.
}
\value{Returns TRUE is attribute was created successfully and FALSE otherwise.}
\references{\url{https://portal.hdfgroup.org/display/HDF5}}
\author{Bernd Fischer}
\seealso{
\code{\link{h5createFile}}, \code{\link{h5createGroup}}, \code{\link{h5createDataset}}, \code{\link{h5read}}, \code{\link{h5write}}, \link{rhdf5}
}
\examples{
h5createFile("ex_createAttribute.h5")
h5write(1:1, "ex_createAttribute.h5","A")
fid <- H5Fopen("ex_createAttribute.h5")
did <- H5Dopen(fid, "A")
h5createAttribute (did, "time", c(1,10))
H5Dclose(did)
H5Fclose(fid)
}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
