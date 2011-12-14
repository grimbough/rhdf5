\name{H5attribute-class}
\Rdversion{1.1}
\docType{class}
\alias{H5attribute-class}
\alias{H5attribute}
\alias{show,H5attribute-method}

\title{Class "H5attribute"}
\description{
A class representing a HDF5 attribute handle.
}
\section{Objects from the Class}{
Objects can be created by calls of \code{\link{H5Acreate}}, \code{\link{H5Aopen}}, \code{\link{H5Aopen_by_idx}}, or \code{\link{H5Aopen_by_name}}.
}
\section{Slots}{
  \describe{
    \item{\code{ID}:}{Object of class \code{"integer"}: Contains the handle of C-type hid_t. }
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "H5attribute")}: Shows the filename, groupname, datatype, and dimensions. }
	 }
}
\references{\url{http://www.hdfgroup.org/HDF5}}
\author{Bernd Fischer}
\seealso{
\link{rhdf5}
}
\examples{
showClass("H5attribute")
}
\keyword{classes}
