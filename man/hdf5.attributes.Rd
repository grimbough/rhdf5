\name{hdf5.attributes}
\alias{hdf5.attributes}
\alias{hdf5.attr}
\alias{hdf5.attr<-}
\title{ Attributes on HDF5 Objects. }
\description{
  The HDF5 package provides the H5A library for the manipulation of
  attributes on HDF5 objects.
}
\usage{
hdf5.attributes(set)
hdf5.attr(set, name)
hdf5.attr(set, name) <- value
}
\arguments{
  \item{set}{ An object of class \code{hdf5.dataset}. }
  \item{name}{The name of an attribute (a character string).}
  \item{value}{The value to associate with the attribute.}
}
\details{
  Even though the HDF5 attributes look a lot like \R attributes I do not
  believe that these functions should become generic. It would make
  little sense (and a lot of work) to try and syncronize HDF5 attributes
  and \R attributes.

  The HDF5 attributes do have the advantage that they are stored with
  the HDF5 datasets and groups and hence have persistence across \R
  sessions. 
}
\value{
  For \code{hdf5.attributes} a named list of attributes is returned.
  For \code{hdf5.attr} the value of the specific attribute is returned.
  For \code{hdf5.attr<-} the \code{value} is returned invisibly.
}
\references{\url{http://hdf.ncsa.uiuc.edu/} }
\author{B. Ellis and R. Gentleman }

\note{
  The following excerpt is taken from the HDF5 online Reference:
  
  The Attribute interface, H5A, is primarily designed to easily allow
  small datasets to be attached to primary datasets as metadata
  information. Additional goals for the H5A interface 
include keeping storage requirement for each attribute to a minimum and
easily sharing attributes among datasets.  

Because attributes are intended to be small objects, large datasets
intended as additional information for a primary dataset should be
stored as supplemental datasets in a group with the 
primary dataset. Attributes can then be attached to the group containing
everything to indicate a particular type of dataset with supplemental
datasets is located in the group. How small 
is "small" is not defined by the library and is up to the user's
interpretation.
}

\seealso{\code{\link[hdf5]{hdf5.dataset}}, \code{\link{attr}} }

\examples{
  mad <- hdf5.open("microarray.h5")
  hdf5.attributes(mad)
  hdf5.group(mad,"chip")
  hdf5.attr(mad$chip, "foo") <- 33
  hdf5.attributes(mad$chip)
  hdf5.attr(mad$chip, "foo")
  hdf5.attr(mad$chip, "bar") <- "aa"
  hdf5.attributes(mad$chip)
}
\keyword{attribute }
