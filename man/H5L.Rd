\name{HDF5 Link Interface}
\alias{H5L}
\alias{H5Lexists}
\alias{H5Lget_info}
\alias{H5Lcreate_external}
\alias{H5Ldelete}
\alias{H5Lcopy}
\alias{H5Lmove}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{HDF5 Link Interface}
\description{
The Link interface, H5L, functions create and manipulate links in an HDF5 group. This interface includes functions that enable the creation and use of user-defined link classes.
}
\usage{
H5Lcreate_external (target_file_name, target_obj_name, link_loc, link_name)
H5Lexists          (h5loc, name)
H5Lget_info        (h5loc, name)
H5Ldelete          (h5loc, name)
H5Lcopy            (h5loc, name, h5loc_dest, name_dest, lcpl = NULL, lapl = NULL )
H5Lmove            (h5loc, name, h5loc_dest, name_dest, lcpl = NULL, lapl = NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{target_file_name}{the relative or absoulte target file name containing the target object.}
  \item{target_obj_name}{the absolute path and name of the target object within the target file.}
  \item{link_loc}{An object of class \code{\link{H5IdComponent}} representing a H5 location identifier (file or group) where the new link is placed.}
  \item{link_name}{The name of the new link.}
  \item{h5loc}{An object of class \code{\link{H5IdComponent}} representing a H5 location identifier (file or group). See \code{\link{H5Fcreate}}, \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}} to create an object of this kind.}
  \item{h5loc_dest}{An object of class \code{\link{H5IdComponent}} representing
  the destination file or group where a copied or moved link should be created.}
  \item{name}{The name of the link to be checked, copied or moved.}
  \item{name_dest}{The name of the link to be created when copying or moving.}
  \item{lcpl}{Link creation property list.}
  \item{lapl}{Link access property list.}
}
\details{
Interface to the HDF5 C-library libhdf5. See \url{https://portal.hdfgroup.org/display/HDF5/Links} for further details.

If \code{name} consists of a relative path containing group names, the function \code{H5Lexists} checks recursively if the links exists which is a different behaviour to the C-function.
}
\value{
\code{H5Lexists} returns boolean TRUE if the link exists and FALSE otherwise.

\code{H5Lget_info} returns a list with the entries of the C-structure \code{H5L_info_t}.
}
\references{\url{https://portal.hdfgroup.org/display/HDF5}}
\author{Bernd Fischer, Mike Smith}
\seealso{
\link{rhdf5}
}
\examples{
# create an hdf5 file and a group
h5createFile("ex_H5L.h5")
h5createGroup("ex_H5L.h5","foo")

# reopen file and get link info
fid <- H5Fopen("ex_H5L.h5")
H5Lexists(fid, "foo")
H5Lexists(fid, "baa")
H5Lget_info(fid, "foo")

H5Ldelete(fid, "foo")
H5Lexists(fid, "foo")

H5Fclose(fid)
}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
