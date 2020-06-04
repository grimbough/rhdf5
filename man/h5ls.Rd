\name{h5ls}
\alias{h5ls}
\alias{h5dump}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{List the content of an HDF5 file.}
\description{
Lists the content of an HDF5 file.
}
\usage{
h5ls   (file,
        recursive = TRUE,
	    all = FALSE, 
	    datasetinfo = TRUE,
	    index_type = h5default("H5_INDEX"),
	    order = h5default("H5_ITER"), 
	    s3 = FALSE, 
	    s3credentials = NULL,
	    native = FALSE)
	    
h5dump (file,
        recursive = TRUE,
	    load = TRUE,
	    all = FALSE,
        index_type = h5default("H5_INDEX"),
        order = h5default("H5_ITER"), 
        s3 = FALSE, 
        s3credentials = NULL,
        ..., native = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{The filename (character) of the file in which the dataset will be located. For advanced programmers it is possible to provide an object of class \code{\link{H5IdComponent}} representing a H5 location identifier (file or group). See \code{\link{H5Fcreate}}, \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}} to create an object of this kind.}
  \item{recursive}{If TRUE, the content of the whole group hierarchy is listed. If FALSE, Only the content of the main group is shown. If a positive integer is provided this indicates the maximum level of the hierarchy that is shown.}
  \item{all}{If TRUE, a longer list of information on each entry is provided.}
  \item{datasetinfo}{If FALSE, datatype and dimensionality information is not provided. This can speed up the content listing for large files.}
  \item{index_type}{See \code{h5const("H5_INDEX")} for possible arguments.}
  \item{order}{See \code{h5const("H5_ITER")} for possible arguments.}
  \item{load}{If TRUE the datasets are read in, not only the header information. Note, that this can cause memory problems for very large files. In this case choose \code{load=FALSE} and load the datasets successively.}
  \item{s3}{Logical value indicating whether the file argument should be 
  treated as a URL to an Amazon S3 bucket, rather than a local file path.}
  \item{s3credentials}{A list of length three, providing the credentials for accessing files in a private Amazon S3 bucket.}
  \item{native}{An object of class \code{logical}. If TRUE, array-like objects are treated as stored in HDF5 row-major rather than R column-major orientation. Using \code{native = TRUE} increases HDF5 file portability between programming languages. A file written with \code{native = TRUE} should also be read with \code{native = TRUE}}.
  \item{\dots}{Arguments passed to \code{\link{h5read}}}
}
\details{
\code{h5ls} lists the content of an HDF5 file including group structure and datasets. It returns the content as a data.frame. You can use \code{h5dump(file="myfile.h5", load=FALSE)} to obtain the dataset information in a hierarchical list structure. Usually the datasets are loaded individually with \code{\link{h5read}}, but you have the possibility to load the complete content of an HDF5 file with \code{h5dump}
}
\value{
\code{h5ls} returns a data.frame with the file content.

\code{h5dump} returns a hierarchical list structure representing the HDF5 group hierarchy. It either returns the datasets within the list structure (\code{load=TRUE}) or it returns a data.frame for each datset with the dataset header information \code{load=FALSE}.
}
\references{\url{https://portal.hdfgroup.org/display/HDF5}}
\author{Bernd Fischer, Mike L. Smith}
\seealso{
\code{\link{h5read}}, \code{\link{h5write}}, \link{rhdf5}
}
\examples{
h5createFile("ex_ls_dump.h5")

# create groups
h5createGroup("ex_ls_dump.h5","foo")
h5createGroup("ex_ls_dump.h5","foo/foobaa")

# write a matrix
B = array(seq(0.1,2.0,by=0.1),dim=c(5,2,2))
attr(B, "scale") <- "liter"
h5write(B, "ex_ls_dump.h5","foo/B")

# list content of hdf5 file
h5ls("ex_ls_dump.h5",all=TRUE)
h5dump("ex_ls_dump.h5")

# list content of an hdf5 file in a public S3 bucket
h5ls(file = "https://rhdf5-public.s3.eu-central-1.amazonaws.com/h5ex_t_array.h5", s3 = TRUE)
h5dump(file = "https://rhdf5-public.s3.eu-central-1.amazonaws.com/h5ex_t_array.h5", s3 = TRUE)
}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
