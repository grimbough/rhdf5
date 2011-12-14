\name{HDF5 Group Interface}
\alias{H5G}
\alias{H5Gcreate}
\alias{H5Gcreate_anon}
\alias{H5Gopen}
\alias{H5Gclose}
\alias{H5Gget_info}
\alias{H5Gget_info_by_idx}
\alias{H5Gget_info_by_name}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{HDF5 Group Interface}
\description{
The Group interface functions create and manipulate groups of objects in an HDF5 file. 
}
\usage{
H5Gcreate      (h5loc, name)
H5Gcreate_anon (h5loc)
H5Gopen        (h5loc, name)
H5Gclose       (h5group)
H5Gget_info    (h5group)
H5Gget_info_by_idx  (h5loc, n, group_name = ".", 
                       index_type = h5default("H5_INDEX"), 
		       order = h5default("H5_ITER"))
H5Gget_info_by_name (h5loc, group_name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h5loc}{An object of class \code{\link{H5file}} or class \code{\link{H5group}}. See \code{\link{H5Fcreate}}, \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}} to create an object of this kind.}
  \item{name}{Name of the group.}
  \item{h5group}{An object of class \code{\link{H5group}}. See \code{\link{H5Gcreate}}, \code{\link{H5Gopen}} to create an object of this kind.}
  \item{n}{Position in the index of the group for which information is retrieved (Counting is 1-based).}
  \item{group_name}{An additional group name specifying the group for which information is sought. It is interpreted relative to \code{h5loc}.}
  \item{index_type}{See \code{h5const("H5_INDEX")} for possible arguments.}
  \item{order}{See \code{h5const("H5_ITER")} for possible arguments.}
}
\details{
Interface to the HDF5 C-library libhdf5. See \url{http://www.hdfgroup.org/HDF5/doc/RM/RM_H5G.html} for further details.
}
\value{
\code{H5Gcreate}, \code{H5Gcreate_anon}, and \code{H5Gopen} return an object of class \code{\link{H5group}}.

\code{H5Gget_info}, \code{H5Gget_info_by_idx}, and \code{H5Gget_info_by_name} return a list with the group information.

The other functions return the standard return value from their respective C-functions.
}
\references{\url{http://www.hdfgroup.org/HDF5}}
\author{Bernd Fischer}
\seealso{
\link{rhdf5}
}
\examples{
fid <- H5Fcreate("ex_H5G.h5")
gid <- H5Gcreate(fid, "foo")
gid
H5Gget_info(gid)
H5Gclose(gid)

H5Gget_info_by_idx(fid,1)
H5Gget_info_by_name(fid,"foo")

H5Fclose(fid)
}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
