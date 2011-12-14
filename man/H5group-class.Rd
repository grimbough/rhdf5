\name{H5group-class}
\Rdversion{1.1}
\docType{class}
\alias{H5group-class}
\alias{H5group}
\alias{show,H5group-method}

\title{Class "H5group"}
\description{
A class representing a HDF5 group handle.
}
\section{Objects from the Class}{
Objects can be created by calls of \code{\link{H5Gcreate}}, \code{\link{H5Gcreate_anon}}, or \code{\link{H5Gopen}}.
}
\section{Slots}{
  \describe{
    \item{\code{ID}:}{Object of class \code{"integer"}: Contains the handle of C-type hid_t. }
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "H5group")}: Shows the filename and groupname.  }
	 }
}
\references{\url{http://www.hdfgroup.org/HDF5}}
\author{Bernd Fischer}
\seealso{
\link{rhdf5}
}
\examples{
showClass("H5group")
}
\keyword{classes}
