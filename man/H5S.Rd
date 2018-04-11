\name{HDF5 Dataspace Interface}
\alias{H5S}
\alias{H5Screate}
\alias{H5Screate_simple}
\alias{H5Scopy}
\alias{H5Sclose}
\alias{H5Sis_simple}
\alias{H5Sget_simple_extent_dims}
\alias{H5Sselect_hyperslab}
\alias{H5Sselect_index}
\alias{H5Sset_extent_simple}
\alias{H5Sunlimited}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{HDF5 Dataspace Interface}
\description{
These functions create and manipulate the dataspace in which to store the elements of a dataset.
}
\usage{
H5Screate                 (type = h5default("H5S"), native = FALSE)
H5Screate_simple          (dims, maxdims, native = FALSE)
H5Scopy                   (h5space)
H5Sclose                  (h5space)
H5Sis_simple              (h5space)
H5Sget_simple_extent_dims (h5space)
H5Sset_extent_simple      (h5space, dims, maxdims)
H5Sselect_hyperslab       (h5space, op = h5default("H5S_SELECT"), 
                           start = NULL, stride = NULL, count = NULL,
                           block = NULL)
H5Sselect_index           (h5space, index)
H5Sunlimited              ()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{See \code{h5const("H5S")} for possible types.}
  \item{dims}{Dimension of the dataspace. This argument is similar to the dim attribute of an array. When viewing the HDF5 dataset with an C-program (e.g. HDFView), the dimensions appear in inverted order, because the fastest changing dimension in R is the first one, and in C its the last one.}
  \item{maxdims}{Maximum extension of the dimension of the dataset in the file. If not provided, it is set to dims.}
  \item{native}{An object of class \code{logical}. If TRUE, array-like objects are treated as stored in HDF5 row-major rather than R column-major orientation. Using \code{native = TRUE} increases HDF5 file portability between programming languages. A file written with \code{native = TRUE} should also be read with \code{native = TRUE}}.
  \item{h5space}{An object of class \code{\link{H5IdComponent}} representing a H5 dataspace identifier. See \code{\link{H5Dget_space}}, \code{\link{H5Screate_simple}}, \code{\link{H5Screate}} to create an object of this kind.}
  \item{index}{A list of integer indices. The length of the list corresponds to the number of dimensions of the HDF5 array.}
  \item{op}{See h5const("H5S_SELECT") for possible arguments.}
  \item{start}{The start coordinate of a hyperslab (similar to subsetting in R). Counting is R-style 1-based.}
  \item{stride}{The stride of the hypercube. Read the introduction \url{http://ftp.hdfgroup.org/HDF5/Tutor/phypecont.html} before using this argument. R behaves like Fortran in this example.}
  \item{count}{The number of blocks to be written.}
  \item{block}{The block size of the hyperslab. Read the introduction \url{http://ftp.hdfgroup.org/HDF5/Tutor/phypecont.html} before using this argument. R behaves like Fortran in this example.}
}
\details{
Interface to the HDF5 C-library libhdf5. See \url{http://www.hdfgroup.org/HDF5/doc/RM/RM_H5S.html} for further details.

As an introduction to use hyperslabs see these tutorials: See these introductions to hyperslabs: \url{http://www.hdfgroup.org/HDF5/Tutor/selectsimple.html}, \url{http://www.hdfgroup.org/HDF5/Tutor/select.html} and \url{http://ftp.hdfgroup.org/HDF5/Tutor/phypecont.html}. Please note that in R the first dimension is the fastest changing dimension. When viewing the HDF5 datasets with any C-program (e.g. HDFView), the order of dimensions is inverted. In the R interface counting starts with 1, whereas in the C-programs (e.g. HDFView) counting starts with 0.

\code{H5Sselect_index}  is not part of the standard HDF5 C interface. It performes an iterative call to H5select_points by iterating through the given index positions. This function avoids a for loop in R. If a list element is NULL, all elements of the respective dimension are considered.
}
\value{
\code{H5Screate}, \code{H5Screate_simple}, and \code{H5Scopy} return an object of class \code{\link{H5IdComponent}} representing a dataspace.

\code{H5Sis_simple} returns a boolean.

\code{H5Sget_simple_extent_dims} returns an integer vector.

\code{H5Sunlimited} is a simple macro to return the constant \code{H5S_UNLIMITED} that can be provided to the \code{maxdims} arguments of \code{H5Screate_simple} to create and extensible dataspace.

The other functions return the standard return value from their respective C-functions.
}
\references{\url{http://www.hdfgroup.org/HDF5}}
\author{Bernd Fischer}
\seealso{
\link{rhdf5}
}
\examples{
sid <- H5Screate_simple(c(10,5,3))
sid
H5Sis_simple(sid)
H5Sget_simple_extent_dims(sid)

# select a subarray (called hyperslab in the hdf5 community).
# The next h5write can use this to write a subarray
H5Sselect_index(sid, list(1:3,2:4,2))

# always close dataspaces after usage to free recources
H5Sclose(sid)
sid
}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
