% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/H5T.R
\name{H5T_enum}
\alias{H5T_enum}
\alias{H5Tenum_create}
\alias{H5Tenum_insert}
\title{Create or modify an HDF5 enum datatype}
\usage{
H5Tenum_create(dtype_id = "H5T_NATIVE_INT")

H5Tenum_insert(dtype_id, name, value)
}
\arguments{
\item{dtype_id}{ID of HDF5 datatype to work with.  For \code{H5Tenum_create}, this
is the identifier of the base data type, and must be an integer e.g.
\code{H5T_NATIVE_INT}. For \code{H5Tenum_insert} this will be a datatype identifier
created by \code{H5Tenum_create}.}

\item{name}{The name of a the new enum member.  This is analogous to a
"level" in an R factor.}

\item{value}{The value of the new member.  Must be compatible with the base
datatype defined by \code{dtype_id}.}
}
\value{
\itemize{
\item \code{H5Tinsert_enum()} returns an character representing the H5 identifier
of the new datatype.
\item \code{H5Tset_precision()} is called for its side-effect of modifying the
existing datatype.  It will invisibly
return \code{TRUE} if this is successful \code{FALSE} if not.
}
}
\description{
Create or modify an HDF5 enum datatype
}
\examples{
tid <- H5Tenum_create(dtype_id = "H5T_NATIVE_UCHAR")
H5Tenum_insert(tid, name = "TRUE", value = 1L)
H5Tenum_insert(tid, name = "FALSE", value = 0L)

}
