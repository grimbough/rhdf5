\name{h5createDataset}
\alias{h5createDataset}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create HDF5 dataset}
\description{R function to create an HDF5 dataset and defining its dimensionality and compression behaviour.
}
\usage{
h5createDataset (file, dataset, 
		dims, maxdims = dims, 
		storage.mode = "double", H5type = NULL, 
		size = NULL, chunk = dims, level = 6,
		fillValue,
                showWarnings=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{The filename (character) of the file in which the dataset will be located. For advanced programmers it is possible to provide an object of class \code{\link{H5IdComponent}} representing a H5 location identifier (file or group). See \code{\link{H5Fcreate}}, \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}} to create an object of this kind.}
  \item{dataset}{Name of the dataset to be created. The name can contain group names, e.g. 'group/dataset', but the function will fail, if the group does not yet exist.}
  \item{dims}{The dimensions of the array as they will appear in the file. Note, the dimensions will appear in inverted order when viewing the file with a C-programm (e.g. HDFView), because the fastest changing dimension in R is the first one, whereas the fastest changing dimension in C is the last one.}
  \item{maxdims}{The maximum extension of the array. Use \code{H5Sunlimited()} to indicate an extensible dimension.}
  \item{storage.mode}{The storage mode of the data to be written. Can be obtained by \code{storage.mode(mydata)}.}
  \item{H5type}{Advanced programmers can specify the datatype of the dataset within the file. See \code{h5const("H5T")} for a list of available datatypes. If \code{H5type} is specified the argument \code{storage.mode} is ignored. It is recommended to use \code{storage.mode}}
  \item{size}{For \code{storage.mode='character'} the maximum string length has to be specified. HDF5 then stores the string as fixed length character vectors. Together with compression, this should be efficient.}
  \item{chunk}{The chunk size used to store the dataset. It is an integer vector of the same length as \code{dims}. This argument is usually set together with a compression property (argument \code{level}). }
  \item{level}{The compression level used. An integer value between 0 (no compression) and 9 (highest and slowest compression).}
  \item{fillValue}{Standard value for filling the dataset. The storage.mode of value has to be convertable to the dataset type by HDF5.}
  \item{showWarnings}{If TRUE (default), a warning is given if the chunk size is equal to the dataset dimension for large compressed datasets.}
}
\details{
Creates a new dataset. in an existing HDF5 file. The function will fail, if the file doesn't exist or if there exists already another dataset with the same name within the specified file.
}
\value{Returns TRUE is dataset was created successfully and FALSE otherwise.}
\references{\url{http://www.hdfgroup.org/HDF5}}
\author{Bernd Fischer}
\seealso{
\code{\link{h5createFile}}, \code{\link{h5createGroup}}, \code{\link{h5read}}, \code{\link{h5write}}, \link{rhdf5}
}
\examples{
h5createFile("ex_createDataset.h5")

# create dataset with compression
h5createDataset("ex_createDataset.h5", "A", c(5,8), storage.mode = "integer", chunk=c(5,1), level=7)

# create dataset without compression
h5createDataset("ex_createDataset.h5", "B", c(5,8), storage.mode = "integer")
h5createDataset("ex_createDataset.h5", "C", c(5,8), storage.mode = "double")

# write data to dataset
h5write(matrix(1:40,nr=5,nc=8), file="ex_createDataset.h5", name="A")
# write second column
h5write(matrix(1:5,nr=5,nc=1), file="ex_createDataset.h5", name="B", index=list(NULL,2))

h5dump("ex_createDataset.h5")
}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
