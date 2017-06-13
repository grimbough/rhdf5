\name{h5write}
\alias{h5read}
\alias{h5readAttributes}
\alias{h5write}
\alias{h5write.default}
\alias{h5writeDataset}
\alias{h5writeDataset.data.frame}
\alias{h5writeDataset.list}
\alias{h5writeDataset.matrix}
\alias{h5writeDataset.integer}
\alias{h5writeDataset.double}
\alias{h5writeDataset.logical}
\alias{h5writeDataset.character}
\alias{h5writeDataset.array}
\alias{h5writeAttribute}
\alias{h5writeAttribute.matrix}
\alias{h5writeAttribute.integer}
\alias{h5writeAttribute.double}
\alias{h5writeAttribute.logical}
\alias{h5writeAttribute.character}
\alias{h5writeAttribute.array}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reads and write object in HDF5 files}
\description{
Reads and writes objects in HDF5 files. This function can be used to read and write either full arrays/vectors or subarrays (hyperslabs) within an existing dataset.
}
\usage{
h5read                      (file, name, index=NULL,
                             start=NULL, stride=NULL, block=NULL, count=NULL,
                             compoundAsDataFrame = TRUE, callGeneric = TRUE,
			     read.attributes = FALSE, drop = FALSE, ...)
h5readAttributes            (file, name)
h5write                     (obj, file, name, ...)
h5write.default             (obj, file, name, 
                             createnewfile = TRUE,
                             write.attributes = FALSE, ...)
h5writeDataset              (obj, h5loc, name, ...)
h5writeDataset.data.frame   (obj, h5loc, name, level=7, DataFrameAsCompound = TRUE)
h5writeDataset.list         (obj, h5loc, name, level=7)
h5writeDataset.matrix       (...)
h5writeDataset.integer      (...)
h5writeDataset.double       (...)
h5writeDataset.logical      (...)
h5writeDataset.character    (...)
h5writeDataset.array        (obj, h5loc, name, index = NULL,
                             start=NULL, stride=NULL, block=NULL, count=NULL,
                             size=NULL, level=7)
h5writeAttribute            (attr, h5obj, name, ...)
h5writeAttribute.matrix     (...)
h5writeAttribute.integer    (...)
h5writeAttribute.double     (...)
h5writeAttribute.logical    (...)
h5writeAttribute.character  (...)
h5writeAttribute.array      (attr, h5obj, name, size)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{The R object to be written.}
  \item{attr}{The R object to be written as an HDF5 attribute.}
  \item{file}{The filename (character) of the file in which the dataset will be located. For advanced programmers it is possible to provide an object of class \code{\link{H5IdComponent}} representing a H5 location identifier (file or group). See \code{\link{H5Fcreate}}, \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}} to create an object of this kind.}
  \item{h5loc}{An object of class \code{\link{H5IdComponent}} representing a H5 location identifier (file or group). See \code{\link{H5Fcreate}}, \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}} to create an object of this kind.}
  \item{h5obj}{An object of class \code{\link{H5IdComponent}} representing a H5 object identifier (file, group, or dataset). See \code{\link{H5Fcreate}}, \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}}, \code{\link{H5Dcreate}}, or \code{\link{H5Dopen}} to create an object of this kind.}
  \item{name}{The name of the dataset in the HDF5 file. The name of the attribute for hwriteAttribute.}
  \item{index}{List of indices for subsetting. The length of the list has to agree with the dimensional extension of the HDF5 array. Each list element is an integer vector of indices. A list element equal to NULL choses all indices in this dimension. Counting is R-style 1-based.}
  \item{start}{The start coordinate of a hyperslab (similar to subsetting in R). Counting is R-style 1-based. This argument is ignored, if index is not NULL.}
  \item{stride}{The stride of the hypercube. Read the introduction \url{http://ftp.hdfgroup.org/HDF5/Tutor/phypecont.html} before using this argument. R behaves like Fortran in this example. This argument is ignored, if index is not NULL.}
  \item{block}{The block size of the hyperslab. Read the introduction \url{http://ftp.hdfgroup.org/HDF5/Tutor/phypecont.html} before using this argument. R behaves like Fortran in this example. This argument is ignored, if index is not NULL.}
  \item{count}{The number of blocks to be written. This argument is ignored, if index is not NULL.}
  \item{level}{The compression level. An integer value between 0 (no compression) and 9 (highest and slowest compression). Only used, if the dataset does not yet exist. See \code{\link{h5createDataset}} to create an dataset.}
  \item{compoundAsDataFrame}{If true, a compound datatype will be coerced to a data.frame. This is not possible, if the dataset is multi-dimensional. Otherwise the compound datatype will be returned as a list. Nested compound data types will be returned as a nested list.}
  \item{DataFrameAsCompound}{If true, a data.frame will be saved as a compound data type. Otherwise it is saved like a list. The advantage of saving a data.frame as a compound data type is that it can be read as a table from python or with a struct-type from C. The disadvantage is that the data has to be rearranged on disk and thus can slow down I/O. If fast reading is required, DataFrameAsCompound=FALSE is recommended.}
  \item{callGeneric}{If TRUE a generic function h5read.classname will be called if it exists depending on the dataset's class attribute within the HDF5 file. This function can be used to convert the standard output of h5read depending on the class attribute. Note that h5read is not a S3 generic function. Dispatching is done based on the HDF5 attribute after the standard h5read function.}
  \item{size}{The length of string data type. Variable lengt strings are not yet supported.}
  \item{createnewfile}{If TRUE, a new file will be created if necessary.}
  \item{read.attributes}{(logical) If TRUE, the HDF5 attributes are read and attached to the respective R object.}
  \item{drop}{(logical) If TRUE, the HDF5 object is read as a vector with NULL dim attributes.}
  \item{write.attributes}{(logical) If TRUE, all R-attributes attached to the object \code{obj} are written to the HDF5 file.}
  \item{\dots}{Further arguments passed to \code{\link{H5Dread}}.}
}
\details{
Read/writes an R object from/to an HDF5 file. If neither of the arguments \code{start, stride, block, count} is specified, the dataset has the same dimension in the HDF5 file and in memory. If the dataset already exists in the HDF5 file, one can read/write subarrays, so called hyperslabs from/to the HDF5 file. The arguments \code{start, stride, block, count} define the subset of the dataset in the HDF5 file that is to be read/written. See these introductions to hyperslabs: \url{http://www.hdfgroup.org/HDF5/Tutor/selectsimple.html}, \url{http://www.hdfgroup.org/HDF5/Tutor/select.html} and \url{http://ftp.hdfgroup.org/HDF5/Tutor/phypecont.html}. Please note that in R the first dimension is the fastest changing dimension.

When viewing the HDF5 datasets with any C-program (e.g. HDFView), the order of dimensions is inverted. In the R interface counting starts with 1, whereas in the C-programs (e.g. HDFView) counting starts with 0.
}
\value{
\code{h5read} returns an array with the data read. 

\code{h5readAttributes} returns a list of all HDF5 attributes of object \code{name}.

\code{h5write} returns 0 if successful.
}
\references{\url{http://www.hdfgroup.org/HDF5}}
\author{Bernd Fischer}
\seealso{
\code{\link{h5ls}}, \code{\link{h5createFile}}, \code{\link{h5createDataset}}, \link{rhdf5}
}
\examples{
h5createFile("ex_hdf5file.h5")

# write a matrix
B = array(seq(0.1,2.0,by=0.1),dim=c(5,2,2))
attr(B, "scale") <- "liter"
h5write(B, "ex_hdf5file.h5","B")

# read a matrix
E = h5read("ex_hdf5file.h5","B")

# write and read submatrix
h5createDataset("ex_hdf5file.h5", "S", c(5,8), storage.mode = "integer", chunk=c(5,1), level=7)
h5write(matrix(1:5,nr=5,nc=1), file="ex_hdf5file.h5", name="S", index=list(NULL,1))
h5read("ex_hdf5file.h5", "S")
h5read("ex_hdf5file.h5", "S", index=list(NULL,2:3))

# list content of hdf5 file
h5ls("ex_hdf5file.h5")

}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
